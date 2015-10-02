{-# LANGUAGE LambdaCase, OverloadedStrings, ScopedTypeVariables, RecordWildCards, DoAndIfThenElse, ConstraintKinds, TypeFamilies #-}
module SimplePlugins where
import           SimplePlugins.Types
import           SimplePlugins.Etc

-- import System.Signal
import           System.FilePath
import           System.FSNotify
import Data.Tagged
-- import Control.Monad.Trans.Either

import           Control.Concurrent
import           Control.Monad.IO.Class
import           Data.Dynamic
import           Data.Foldable                   (traverse_)
-- import Data.Coerce
-- import           Data.IORef
-- import Control.Exception (getMaskingState) 
-- import Control.Exception (throw, AsyncException(UserInterrupt)) 

import           DynFlags
import           GHC            -- TODO import explicitly 
-- import HscTypes (SourceError) 
import           GHC.Paths
import           Linker
import           Packages


directoryWatcher :: Chan ReloadEvent -> LoaderConfig -> IO ()
directoryWatcher directoryChannel loaderConfig@LoaderConfig{..} = withManager $ \manager -> do
 channel <- newChan
 -- start a watching job (in the background)
 _stopListening <- watchTreeChan
     manager
     _pluginDirectory
     ((&&) <$> (const True) <*> (eventPredicate loaderConfig))
     channel
 -- Keep the watcher alive forever, injecting the filesystem event into a reload event 
 keepAlive$ do
  e <- readChan channel
  writeChan directoryChannel (Right e) 

eventPredicate :: LoaderConfig -> Event -> Bool 
eventPredicate LoaderConfig{..} = \case 
 Modified path _ -> takeExtension path `elem` _pluginExtensions
 Added    path _ -> takeExtension path `elem` _pluginExtensions
 Removed  path _ -> takeExtension path `elem` _pluginExtensions


pluginUpdater :: Chan (PluginEvent plugin) -> UpdatePlugin plugin -> IO ()
pluginUpdater pluginChannel updatePlugin = keepAlive$ do
  readChan pluginChannel >>= updatePlugin

-- | converts a stream of file system changes to a stream of plugins.  
pluginReloader
 :: forall plugin m. (IsPlugin plugin, Ghc ~ m)
 => Chan ReloadEvent            -- ^ reads from 
 -> Chan (PluginEvent plugin)   -- ^ writes to 
 -> SignalHandlerInstaller
 -> LoaderConfig
 -> GhcConfig m
 -> Identifier plugin
 -> IO ()
pluginReloader reloadChannel pluginChannel installSignalHandler loaderConfig ghcConfig identifier = do

 threadChannel <- newChan

 -- the ghc thread has finished running, or no ghc thread is running 
 let notRunning          = writeChan threadChannel Nothing
 -- a ghc thread has started running from this thread 
 let yesRunning threadId = writeChan threadChannel (Just threadId) 
 -- if a ghc thread is running, then kill it
 let stopRunning         = readChan threadChannel >>= traverse_ killThread
 -- 
 let startRunning        = forkIO$ do
       plugin' <- withGHCSession installSignalHandler loaderConfig ghcConfig $ do
                      recompileTargets ghcConfig identifier -- TODO event
       writeChan pluginChannel plugin'
       threadDelay (5*1000*1000) 
       notRunning               -- the thread is about to exit 

 notRunning

 keepAlive$ do
   -- blocks on reading from the channel,
   -- while a plug-in is being reloaded in the child's thread
   _event <- readChan reloadChannel
   -- rather than waiting for the plug-in to finish reloading,
   -- only to then reload a new plug-in, 
   -- just kill the thread reloading it...
   stopRunning
   -- ... and spawn a new one. 
   ghcThread <- startRunning
   yesRunning ghcThread

{- | sets the right 'DynFlags'.   

see <https://github.com/ghc/ghc/blob/089b72f524a6a7564346baca9595fcd07081ec40/compiler/main/DynFlags.hs#L694 DynFlags>

see <https://github.com/ghc/ghc/blob/06d46b1e4507e09eb2a7a04998a92610c8dc6277/compiler/main/GHC.hs#L367 defaultErrorHandler> 


-}
withGHCSession :: (Ghc ~ m) => SignalHandlerInstaller -> LoaderConfig -> GhcConfig m -> m a -> IO a
-- withGHCSession :: (GhcMonad m) => LoaderConfig -> GhcConfig -> m a -> IO a 
withGHCSession installSignalHandler LoaderConfig{..} GhcConfig{..} action = do
 defaultErrorHandler _ghcFatalMessager (FlushOut _ghcFlushOut) $ (runGhc') (Just libdir) $ do -- TODO runGhc installSignalHandler 

  -- NOTE a hack. must run after `initGhcMonad` (which is run in `runGhc`) 
  -- my vague guess, the GHC API assumes the thread that runs runGhc is the main thread 
  liftIO$ installSignalHandler

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



-- | Recompiles the current targets (that have been set by 'setTargets')
recompileTargets :: (IsPlugin plugin, GhcMonad m) => GhcConfig m -> Identifier plugin -> m (PluginEvent plugin)
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

