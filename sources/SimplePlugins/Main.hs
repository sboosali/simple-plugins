{-# LANGUAGE RecordWildCards, NamedFieldPuns    #-}
module SimplePlugins.Main where 

import           SimplePlugins
import           SimplePlugins.Types 
import           SimplePlugins.Etc

import System.Signal
-- import qualified SlaveThread as Slave 
-- import           System.FSNotify

import Control.Concurrent
import Control.Exception

import           GHC (Ghc)


launchReloader
 :: (IsPlugin plugin)
 => LoaderConfig
 -> GhcConfig Ghc
 -> UpdatePlugin plugin
 -> Identifier plugin
 -> PluginReloader
launchReloader loaderConfig ghcConfig updatePlugin identifier = do

 let fork = forkIO              --  Slave.fork

 filenameChannel <- newChan
 pluginChannel <- newChan

 mainThread <- myThreadId

 _watcherThread  <- fork$ directoryWatcher filenameChannel loaderConfig

 _updaterThread  <- fork$ pluginUpdater pluginChannel updatePlugin

 _reloaderThread <- fork$ pluginReloader filenameChannel pluginChannel (userInterruptInstaller mainThread) loaderConfig ghcConfig identifier

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

