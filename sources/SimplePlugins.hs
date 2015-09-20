{-# LANGUAGE LambdaCase, OverloadedStrings, ScopedTypeVariables, RecordWildCards          #-}
module SimplePlugins where
import           SimplePlugins.Types
import           SimplePlugins.Etc

import           System.FilePath
import           System.FSNotify

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Dynamic
import           Data.IORef

import           DynFlags
import           GHC
import           GHC.Paths
import           Linker
import           Packages



eventPredicate :: LoaderConfig -> Event -> Bool 
eventPredicate LoaderConfig{..} = \case 
 Modified path _ -> takeExtension path `elem` _pluginExtensions
 Added    path _ -> takeExtension path `elem` _pluginExtensions
 Removed  path _ -> takeExtension path `elem` _pluginExtensions

directoryWatcher :: LoaderConfig -> IO (Chan Event)
directoryWatcher loaderConfig@LoaderConfig{..} = do

    eventChan <- newChan
    _ <- forkIO $ withManager $ \manager -> do
        -- start a watching job (in the background)
        let watchDirectory = _pluginDirectory
        _stopListening <- watchTreeChan
            manager
            watchDirectory
            (eventPredicate loaderConfig)
            eventChan
        -- Keep the watcher alive forever
        forever $ threadDelay 10000000

    return eventChan



recompiler :: (Typeable plugin) => proxy plugin -> LoaderConfig -> IO ()
recompiler proxy loaderConfig = withGHCSession loaderConfig $ do
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

    mainDone  <- liftIO$ newIORef False
    -- Start with a full MVar so we recompile right away.
    recompile <- liftIO$ newMVar ()

    -- Watch for changes and recompile whenever they occur
    watcher <- liftIO$ directoryWatcher loaderConfig
    _ <- liftIO . forkIO . forever $ do
        readChan watcher >>= print

        putMVar recompile ()
        mainIsDone <- readIORef mainDone
        unless mainIsDone $ killThread mainThreadId

    -- Start up the app
    forever$ do
        _ <- liftIO $ takeMVar recompile
        liftIO$ writeIORef mainDone False
        recompileTargets proxy
        liftIO$ writeIORef mainDone True



withGHCSession :: LoaderConfig -> Ghc () -> IO ()
withGHCSession LoaderConfig{..} action = do
 defaultErrorHandler _ghcFatalMessager (FlushOut _ghcFlushOut) $ runGhc (Just libdir) $ do

  let pluginPath = _pluginDirectory ++ "/" ++ _pluginFile

  -- Get the default dynFlags
  dflags0 <- getSessionDynFlags

  -- If there's a sandbox, add its package DB
  let packageDBs = [PkgConfFile _sandboxPackageDB, PkgConfFile _inplacePackageDB]
  dflags1 <- return$ dflags0 { extraPkgConfs = (packageDBs ++) . extraPkgConfs dflags0 }

  -- If this is a stack project, add its package DBs
  dflags2 <- return dflags1

  -- Make sure we're configured for live-reload, and turn off the GHCi sandbox
  -- since it breaks OpenGL/GUI usage
  -- we have to use HscInterpreted and LinkInMemory.
  -- otherwise it would compile target.hs in the current directory
  -- and leave target.hi and target.o files,
  -- which we would not be able to load in the interpreted mode.
  -- CompManager is like {{ghc --make}}
  let dflags3 = dflags2 { hscTarget = HscInterpreted
                        , ghcLink   = LinkInMemory
                        , ghcMode   = CompManager
                        , importPaths = [dropFileName pluginPath]
                        , verbosity   = _ghcCompilationVerbosity
                        } `gopt_unset` Opt_GhciSandbox
                        -- `gopt_set` Opt_BuildingCabalPackage -- ?

  -- We must set dynflags before calling initPackages or any other GHC API
  _ <- setSessionDynFlags dflags3

  -- Initialize the package database
  (dflags4, _) <- liftIO$ initPackages dflags3

  -- Initialize the dynamic linker
  liftIO$ initDynLinker dflags4

  -- Set the given filename as a compilation target
  setTargets =<< sequence [guessTarget pluginPath Nothing]

  action


-- Recompiles the current targets
recompileTargets :: (Typeable plugin) => proxy plugin -> Ghc ()
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
        let typeRep_importedPlugin = typeRep (undefined :: Maybe Plugin)
        let typeRep_proxiedPlugin = typeRep _proxy
        let typeRep_loadedPlugin = dynTypeRep dynamic
        liftIO$ do
         putStrLn ""
         print $ showTypeRep typeRep_importedPlugin
         print $ showTypeRep typeRep_proxiedPlugin
         print $ showTypeRep typeRep_loadedPlugin
         print $ typeRep_importedPlugin == typeRep_loadedPlugin
         putStrLn ""
        case fromDynamic dynamic of
         Nothing -> liftIO$ do
          putStrLn$ s"plugin has wrong type"
         Just  plugin -> liftIO$ do
          putStrLn$ s"plugin is being reloaded.."
          -- reloadPlugin proxy plugin
          print (plugin::Plugin)

