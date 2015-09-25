{-# LANGUAGE LambdaCase, OverloadedStrings, ScopedTypeVariables, RecordWildCards, DoAndIfThenElse, ConstraintKinds, GeneralizedNewtypeDeriving #-}
module SimplePlugins where
import           SimplePlugins.Types
-- import           SimplePlugins.Etc

import           System.FilePath
import           System.FSNotify
import Data.Tagged
-- import Control.Monad.Trans.Either

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Dynamic
-- import           Data.IORef
import Control.Exception (getMaskingState) 

import           DynFlags
import           GHC            -- TODO import explicitly 
-- import HscTypes (SourceError) 
import           GHC.Paths
import           Linker
import           Packages


pluginReloader :: (IsPlugin plugin) => Chan Event -> Chan (Maybe plugin) -> LoaderConfig -> GhcConfig -> Identifier plugin -> IO ()
pluginReloader watcherChannel pluginChannel loaderConfig ghcConfig identifier
 = withGHCSession loaderConfig ghcConfig $ forever$ do
       _event <- liftIO$ readChan watcherChannel  -- blocks on reading from the channel                       
       plugin' <- recompileTargets ghcConfig identifier
       liftIO$ writeChan pluginChannel plugin'
       liftIO$ threadDelay (1*1000*1000) 


directoryWatcher :: Chan Event -> LoaderConfig -> IO ()
directoryWatcher directoryChannel loaderConfig@LoaderConfig{..} = withManager $ \manager -> do
 -- start a watching job (in the background)
 _stopListening <- watchTreeChan
     manager
     _pluginDirectory
     ((&&) <$> (const True) <*> (eventPredicate loaderConfig))
     directoryChannel 
 -- Keep the watcher alive forever
 forever$ threadDelay (1*1000*1000) 

eventPredicate :: LoaderConfig -> Event -> Bool 
eventPredicate LoaderConfig{..} = \case 
 Modified path _ -> takeExtension path `elem` _pluginExtensions
 Added    path _ -> takeExtension path `elem` _pluginExtensions
 Removed  path _ -> takeExtension path `elem` _pluginExtensions


pluginUpdater :: Chan (Maybe plugin) -> (Maybe plugin -> IO ()) -> IO ()
pluginUpdater pluginChannel updatePlugin = forever$ do
  readChan pluginChannel >>= updatePlugin
  threadDelay (1*1000*1000)   

-- | converts a stream of file system changes to a stream of plugins 
pluginWatcher :: (IsPlugin plugin) => Chan Event -> Chan (Maybe plugin) -> LoaderConfig -> GhcConfig -> Identifier plugin -> IO ()
pluginWatcher watcherChannel pluginChannel loaderConfig ghcConfig identifier = forever$ do
   _event <- readChan watcherChannel  -- blocks on reading from the channel                       
   forkIO$ do
       plugin' <- withGHCSession loaderConfig ghcConfig $ do
           liftIO$ print =<< getMaskingState
           recompileTargets ghcConfig identifier -- event
       writeChan pluginChannel plugin'
   threadDelay (1*1000*1000) 


withGHCSession :: LoaderConfig -> GhcConfig -> Ghc a -> IO a 
withGHCSession LoaderConfig{..} GhcConfig{..} action = do
 defaultErrorHandler _ghcFatalMessager (FlushOut _ghcFlushOut) $ runGhc (Just libdir) $ do

  let pluginPath = _pluginDirectory ++ "/" ++ _pluginFile

  -- Get the default dynFlags
  dflags0 <- getSessionDynFlags

  -- If there's a sandbox, add its package databases
  let packageDBs = [PkgConfFile _sandboxPackageDB, PkgConfFile _inplacePackageDB]
  let dflags1 = dflags0 { extraPkgConfs = (packageDBs ++) . extraPkgConfs dflags0 }

  -- Make sure we're configured for live-reload, and turn off the GHCi sandbox
  -- since it breaks OpenGL/GUI usage
  -- we have to use HscInterpreted and LinkInMemory.
  -- otherwise it would compile target.hs in the current directory
  -- and leave target.hi and target.o files,
  -- which we would not be able to load in the interpreted mode.
  -- CompManager is like {{ghc --make}}
  let dflags2 = dflags1 { hscTarget = HscInterpreted
                        , ghcLink   = LinkInMemory
                        , ghcMode   = CompManager
                        , importPaths = [dropFileName pluginPath]
                        , verbosity   = _ghcCompilationVerbosity
                        } `gopt_unset` Opt_GhciSandbox
                        -- `gopt_set` Opt_BuildingCabalPackage -- ?

  -- We must set dynflags before calling initPackages or any other GHC API
  _ <- setSessionDynFlags dflags2

  -- Initialize the package database
  (dflags3, _) <- liftIO$ initPackages dflags2

  -- Initialize the dynamic linker
  liftIO$ initDynLinker dflags3

  -- Set the given filename as a compilation target
  setTargets =<< sequence [guessTarget pluginPath Nothing]

  action



-- Recompiles the current targets
recompileTargets :: forall plugin. (IsPlugin plugin) => GhcConfig -> Identifier plugin -> Ghc (Maybe plugin)
recompileTargets GhcConfig{..} (Tagged identifier) = let _ghcPrintSourceError' e = _ghcPrintSourceError e >> return Nothing in handleSourceError _ghcPrintSourceError' $ do

-- FilePath ->
--   -- Set the given filename as a compilation target
--   setTargets =<< sequence [guessTarget pluginPath Nothing]

 -- Get the dependencies of the main target
 rawDependencies <- depanal [] False

 -- Reload the main target
 loadSuccess <- load LoadAllTargets
 if   failed loadSuccess
 then return Nothing
 else do

     -- We must parse and typecheck modules before they'll be available for usage
     _parsedDependencies      <- traverse parseModule     rawDependencies
     _typecheckedDependencies <- traverse typecheckModule _parsedDependencies

     -- Load the dependencies of the main target
     setContext $ (IIModule . ms_mod_name) <$> rawDependencies 

     -- load the target file's identifier
     dynamic <- dynCompileExpr identifier 
     return$ fromDynamic dynamic

