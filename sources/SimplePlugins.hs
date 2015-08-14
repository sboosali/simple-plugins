{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module SimplePlugins where
import           SimplePlugins.Types    ()

import           System.FilePath
import           System.FSNotify

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Dynamic
import           Data.IORef
import           Data.Typeable          ()

import           DynFlags
import           Fingerprint
import           GHC
import           GHC.Paths
import           Linker
import           Outputable
import           Packages

-- for OverloadedStrings
s :: String -> String
s = id

directoryWatcher :: IO (Chan Event)
directoryWatcher = do
    let predicate event = case event of
            Modified path _ -> takeExtension path `elem` [".hs"]
            _               -> False
    eventChan <- newChan
    _ <- forkIO $ withManager $ \manager -> do
        -- start a watching job (in the background)
        let watchDirectory = "."
        _stopListening <- watchTreeChan
            manager
            watchDirectory
            predicate
            eventChan
        -- Keep the watcher alive forever
        forever $ threadDelay 10000000

    return eventChan



recompiler :: proxy plugin -> FilePath -> [FilePath] -> IO ()
recompiler proxy mainFileName importPaths' = withGHCSession mainFileName importPaths' $ do
    mainThreadId <- liftIO myThreadId

    {-
    Watcher:
        Tell the main thread to recompile.
        If the main thread isn't done yet, kill it.
    Compiler:
        Wait for the signal to recompile.
        Before recompiling & running, mark that we've started,
        and after we're done running, mark that we're done.
    -}

    mainDone  <- liftIO $ newIORef False
    -- Start with a full MVar so we recompile right away.
    recompile <- liftIO $ newMVar ()

    -- Watch for changes and recompile whenever they occur
    watcher <- liftIO directoryWatcher
    _ <- liftIO . forkIO . forever $ do
        _ <- readChan watcher
        putMVar recompile ()
        mainIsDone <- readIORef mainDone
        unless mainIsDone $ killThread mainThreadId

    -- Start up the app
    forever $ do
        _ <- liftIO $ takeMVar recompile
        liftIO $ writeIORef mainDone False
        recompileTargets proxy
        liftIO $ writeIORef mainDone True


withGHCSession :: FilePath -> [FilePath] -> Ghc () -> IO ()
withGHCSession mainFileName extraImportPaths action = do
    defaultErrorHandler defaultFatalMessager defaultFlushOut $ runGhc (Just libdir) $ do
        -- Add the main file's path to the import path list
        let mainFilePath   = dropFileName mainFileName
            allImportPaths = mainFilePath:extraImportPaths

        -- Get the default dynFlags
        dflags0 <- getSessionDynFlags

        let sandboxPackageDB = "/Users/sambo/voice/simple-plugins/.cabal-sandbox/x86_64-osx-ghc-7.10.1-packages.conf.d"
        -- If there's a sandbox, add its package DB
        let packageDBs = [PkgConfFile sandboxPackageDB]
        dflags1 <- return$ dflags0 { extraPkgConfs = (packageDBs ++) . extraPkgConfs dflags0 }

        -- If this is a stack project, add its package DBs
        dflags2 <- return dflags1

        -- Make sure we're configured for live-reload, and turn off the GHCi sandbox
        -- since it breaks OpenGL/GUI usage
-- we have to use HscInterpreted and LinkInMemory; otherwise it would compile target.hs in the current directory and leave target.hi and target.o files, which we would not be able to load in the interpreted mode.
-- CompManager: like ghc --make
-- verbosity is 3 (like @ghc -v@), for debugging
        let dflags3 = dflags2 { hscTarget = HscInterpreted
                              , ghcLink   = LinkInMemory
                              , ghcMode   = CompManager
                              , importPaths = allImportPaths
                              , verbosity = 3
                              } `gopt_unset` Opt_GhciSandbox
                                -- `gopt_set` Opt_BuildingCabalPackage -- ?

        -- We must set dynflags before calling initPackages or any other GHC API
        _ <- setSessionDynFlags dflags3

        -- Initialize the package database
        (dflags4, _) <- liftIO$ initPackages dflags3

        -- Initialize the dynamic linker
        liftIO $ initDynLinker dflags4

        -- Set the given filename as a compilation target
        setTargets =<< sequence [guessTarget mainFileName Nothing]

        action


-- Recompiles the current targets
recompileTargets :: proxy plugin -> Ghc ()
recompileTargets _proxy = handleSourceError printException $ do
    -- Get the dependencies of the main target
    graph <- depanal [] False

    -- Reload the main target
    loadSuccess <- load LoadAllTargets
    unless (failed loadSuccess) $ do
        -- We must parse and typecheck modules before they'll be available for usage
        forM_ graph (typecheckModule <=< parseModule)

        -- Load the dependencies of the main target
        setContext $ (IIModule . ms_mod_name) <$> graph

        -- load the target file's "plugin" identifier
        dynamic <- dynCompileExpr "plugin"
        case fromDynamic dynamic of
         Nothing -> do
          liftIO$ putStrLn$ s"plugin has wrong type"
         Just  plugin -> do
          liftIO$ putStrLn$ s"plugin is being reloaded.."
          -- liftIO$ reloadPlugin proxy plugin
          liftIO$ print (plugin::String)

showTypeRep :: TypeRep -> (String, String, String, Fingerprint)
showTypeRep tr = (tyConName tc, tyConModule tc, tyConPackage tc, tyConFingerprint tc)
 where tc = typeRepTyCon tr

-- reloadPlugin :: Show a => Plugin a -> IO ()
-- reloadPlugin :: proxy plugin -> Plugin a -> IO ()
-- reloadPlugin _ (Plugin value) = do
--  print value

-- a helper from interactive-diagrams to print out GHC API values,
-- useful while debugging the API.
-- | Outputs any value that can be pretty-printed using the default style
output :: (GhcMonad m, MonadIO m) => Outputable a => a -> m ()
output a = do
    dfs <- getSessionDynFlags
    let style = defaultUserStyle
    let cntx  = initSDocContext dfs style
    liftIO $ print $ runSDoc (ppr a) cntx
