{-# LANGUAGE OverloadedStrings, LambdaCase, ScopedTypeVariables #-}
module SimplePlugins where

import System.FSNotify
import System.FilePath

import GHC
import Linker
import Packages
import DynFlags
import GHC.Paths
import Outputable
import Data.IORef
import Control.Monad
import Control.Concurrent
import Control.Monad.IO.Class
import Data.Dynamic

directoryWatcher :: IO (Chan Event)
directoryWatcher = do
    let predicate event = case event of
            Modified path _ -> takeExtension path `elem` [".hs", ".vert", ".frag", ".pd"]
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



recompiler :: FilePath -> [FilePath] -> IO ()
recompiler mainFileName importPaths' = withGHCSession mainFileName importPaths' $ do
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
        recompileTargets
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
        dflags1 <- return$ dflags0 { extraPkgConfs = (PkgConfFile sandboxPackageDB :) . extraPkgConfs dflags0 }

        -- If this is a stack project, add its package DBs
        dflags2 <- return dflags1

        -- Make sure we're configured for live-reload, and turn off the GHCi sandbox
        -- since it breaks OpenGL/GUI usage
-- we have to use HscInterpreted and LinkInMemory; otherwise it would compile target.hs in the current directory and leave target.hi and target.o files, which we would not be able to load in the interpreted mode.
-- CompManager: like ghc --make
        let dflags3 = dflags2 { hscTarget = HscInterpreted
                              , ghcLink   = LinkInMemory
                              , ghcMode   = CompManager
                              , importPaths = allImportPaths
                              } `gopt_unset` Opt_GhciSandbox
        
        -- We must set dynflags before calling initPackages or any other GHC API
        _ <- setSessionDynFlags dflags3

        -- Initialize the package database
        (dflags4, _) <- liftIO $ initPackages dflags3

        -- Initialize the dynamic linker
        liftIO $ initDynLinker dflags4 

        -- Set the given filename as a compilation target
        setTargets =<< sequence [guessTarget mainFileName Nothing]

        action

-- Recompiles the current targets
recompileTargets :: Ghc ()
recompileTargets = handleSourceError printException $ do
    -- Get the dependencies of the main target
    graph <- depanal [] False

    -- Reload the main target
    loadSuccess <- load LoadAllTargets
    unless (failed loadSuccess) $ do
        -- We must parse and typecheck modules before they'll be available for usage
        forM_ graph (typecheckModule <=< parseModule)
        
        -- Load the dependencies of the main target
        setContext $ map (IIModule . ms_mod_name) graph

        -- load the target file's "plugin" identifier
        dynamic <- dynCompileExpr "plugin"
        case fromDynamic dynamic of
         Nothing -> do
          liftIO$ putStrLn$ s"plugin has wrong type"
         Just  (plugin::String) -> do
          liftIO$ putStrLn$ s"plugin is being reloaded.."
          -- liftIO$ reloadPlugin plugin
          liftIO$ print plugin

s :: String -> String
s = id

data Plugin = Plugin String

reloadPlugin :: Plugin -> IO ()
reloadPlugin (Plugin value) = do
 print value


-- a helper from interactive-diagrams to print out GHC API values, 
-- useful while debugging the API.
-- | Outputs any value that can be pretty-printed using the default style
output :: (GhcMonad m, MonadIO m) => Outputable a => a -> m ()
output a = do
    dfs <- getSessionDynFlags
    let style = defaultUserStyle
    let cntx  = initSDocContext dfs style
    liftIO $ print $ runSDoc (ppr a) cntx
