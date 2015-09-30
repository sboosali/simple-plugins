{-# LANGUAGE LambdaCase, OverloadedStrings, ScopedTypeVariables, RecordWildCards, DoAndIfThenElse, ConstraintKinds, TypeFamilies #-}
module SimplePlugins where
import           SimplePlugins.Types
import           SimplePlugins.Etc

-- import System.Signal
import           System.FilePath
import           System.FSNotify
import Data.Tagged
-- import Control.Monad.Trans.Either

-- import Data.Coerce
import           Control.Concurrent
import           Control.Monad.IO.Class
import           Data.Dynamic
-- import           Data.IORef
-- import Control.Exception (getMaskingState) 
-- import Control.Exception (throw, AsyncException(UserInterrupt)) 

import           DynFlags
import           GHC            -- TODO import explicitly 
-- import HscTypes (SourceError) 
import           GHC.Paths
import           Linker
import           Packages


-- -- only runs once 
-- pluginReloaderGhc :: (IsPlugin plugin, m ~ Ghc) => Chan Event -> Chan (Maybe plugin) -> LoaderConfig -> GhcConfig m -> Identifier plugin -> IO ()
-- pluginReloaderGhc filenameChannel pluginChannel loaderConfig ghcConfig identifier
--  = withGHCSession loaderConfig ghcConfig $ forever$ do
--        _event <- liftIO$ readChan filenameChannel  -- blocks on reading from the channel                       
--        plugin' <- recompileTargets ghcConfig identifier
--        liftIO$ writeChan pluginChannel plugin'
--        liftIO$ keepAlive$ return() 


directoryWatcher :: Chan Event -> LoaderConfig -> IO ()
directoryWatcher directoryChannel loaderConfig@LoaderConfig{..} = withManager $ \manager -> do
 -- start a watching job (in the background)
 _stopListening <- watchTreeChan
     manager
     _pluginDirectory
     ((&&) <$> (const True) <*> (eventPredicate loaderConfig))
     directoryChannel 
 -- Keep the watcher alive forever
 keepAlive$ return() 

eventPredicate :: LoaderConfig -> Event -> Bool 
eventPredicate LoaderConfig{..} = \case 
 Modified path _ -> takeExtension path `elem` _pluginExtensions
 Added    path _ -> takeExtension path `elem` _pluginExtensions
 Removed  path _ -> takeExtension path `elem` _pluginExtensions


pluginUpdater :: Chan (Maybe plugin) -> (Maybe plugin -> IO ()) -> IO ()
pluginUpdater pluginChannel updatePlugin = keepAlive$ do
  readChan pluginChannel >>= updatePlugin

-- | converts a stream of file system changes to a stream of plugins.  
pluginReloader
 :: forall plugin m. (IsPlugin plugin, Ghc ~ m)
 => Chan Event
 -> Chan (Maybe plugin)
 -> SignalHandlerInstaller
 -> LoaderConfig
 -> GhcConfig m
 -> Identifier plugin
 -> IO ()
pluginReloader filenameChannel pluginChannel installSignalHandler loaderConfig ghcConfig identifier = keepAlive$ do
   _event <- readChan filenameChannel  -- blocks on reading from the channel                       
   plugin' <- withGHCSession installSignalHandler loaderConfig ghcConfig $ do
               recompileTargets ghcConfig identifier -- TODO event
   writeChan pluginChannel plugin'

{- | sets the right 'DynFlags'.   

see <https://github.com/ghc/ghc/blob/089b72f524a6a7564346baca9595fcd07081ec40/compiler/main/DynFlags.hs#L694 DynFlags>

see <https://github.com/ghc/ghc/blob/06d46b1e4507e09eb2a7a04998a92610c8dc6277/compiler/main/GHC.hs#L367 defaultErrorHandler> 


-}
withGHCSession :: (Ghc ~ m) => SignalHandlerInstaller -> LoaderConfig -> GhcConfig m -> m a -> IO a
-- withGHCSession :: (GhcMonad m) => LoaderConfig -> GhcConfig -> m a -> IO a 
withGHCSession installSignalHandler LoaderConfig{..} GhcConfig{..} action = do
 defaultErrorHandler _ghcFatalMessager (FlushOut _ghcFlushOut) $ (runGhc') (Just libdir) $ do

  -- NOTE a hack. must run after `initGhcMonad` (which is run in `runGhc`) 
  -- my vague guess, the GHC API assumes the thread that runs runGhc is the main thread 
  liftIO$ installSignalHandler [] 

  -- derived configuration 
  let pluginPath = _pluginDirectory ++ "/" ++ _pluginFile
  let packageDBs = [PkgConfFile _sandboxPackageDB, PkgConfFile _inplacePackageDB]

  -- Get the default dynFlags
  dflags0 <- getSessionDynFlags

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
                        -- , log_action  = TODO 
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
recompileTargets :: (IsPlugin plugin, GhcMonad m) => GhcConfig m -> Identifier plugin -> m (Maybe plugin)
-- recompileTargets :: (IsPlugin plugin, GhcMonad m) => GhcConfig -> Identifier plugin -> m (Maybe plugin)
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

{-| TODO
You can set the log_action field of the session's DynFlags to a custom
handler. Its value is

type LogAction = Severity -> SrcSpan -> PprStyle -> Message -> IO ()

The Severity parameter will let you tell whether a message is a
warning or an error.

-} 

