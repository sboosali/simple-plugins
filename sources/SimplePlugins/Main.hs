{-# LANGUAGE RecordWildCards, NamedFieldPuns    #-}
module SimplePlugins.Main where 

import           SimplePlugins
import           SimplePlugins.Types 
import           SimplePlugins.Etc

import System.Signal
-- import qualified SlaveThread as Slave 

import Control.Monad
import Control.Concurrent
import Control.Exception

import           GHC (Ghc)


defaultLaunchReloader
 :: (IsPlugin plugin)
 => FilePath
 -> UpdatePlugin plugin
 -> Identifier plugin
 -> PluginReloader
defaultLaunchReloader filepath updatePlugin identifier
 = join$ launchReloader (defaultLoaderConfig filepath) defaultGhcConfig updatePlugin identifier <$> myThreadId <*> newChan <*> newChan
--TODO must pass main thread, or any parent thread?

launchReloader
 :: (IsPlugin plugin)
 => LoaderConfig
 -> GhcConfig Ghc
 -> UpdatePlugin plugin
 -> Identifier plugin
 -> ThreadId
 -> Chan ReloadEvent
 -> Chan (PluginEvent plugin)
 -> PluginReloader
launchReloader loaderConfig ghcConfig updatePlugin identifier mainThread reloadChannel pluginChannel = do

 writeChan reloadChannel (Left ()) -- reload plug-in at least once on startup

 _watcherThread  <- forkIO$ directoryWatcher reloadChannel loaderConfig

 _updaterThread  <- forkIO$ pluginUpdater pluginChannel updatePlugin

 _reloaderThread <- forkIO$ pluginReloader reloadChannel pluginChannel (userInterruptInstaller mainThread) loaderConfig ghcConfig identifier

 keepAlive$ return() 
 -- pluginThreads

userInterruptInstaller :: ThreadId -> SignalHandlerInstaller 
userInterruptInstaller mainThread 
 = installHandler sigINT (\_signal -> handleUserInterrupt mainThread)

-- without this, the program user interrupt http://neilmitchell.blogspot.com/2015/05/handling-control-c-in-haskell.html
-- C-c causes "example: user interrupt" to be printed, doesn't cause determination 
handleUserInterrupt :: ThreadId -> IO a
handleUserInterrupt t = do
 throwTo t UserInterrupt
 throw UserInterrupt

