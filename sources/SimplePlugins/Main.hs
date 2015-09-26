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



-- TODO ignores user interrupt http://neilmitchell.blogspot.com/2015/05/handling-control-c-in-haskell.html
-- C-c causes "example: user interrupt" not termination 
launchReloader
 :: (IsPlugin plugin)
 => LoaderConfig
 -> GhcConfig SaferGhc
 -> UpdatePlugin plugin
 -> Identifier plugin
 -> PluginReloader
launchReloader loaderConfig ghcConfig updatePlugin identifier = do

 let fork = forkIO              --  Slave.fork

 watcherChannel <- newChan
 pluginChannel <- newChan

 _mainThread <- myThreadId

 _watcherThread  <- fork$ directoryWatcher watcherChannel loaderConfig

 _updaterThread  <- fork$ pluginUpdater pluginChannel updatePlugin

 _reloaderThread <- fork$ pluginWatcher watcherChannel pluginChannel loaderConfig ghcConfig identifier

 -- `installInterruptHandler` must run after `initGhcMonad`
 -- let installInterruptHandler = return()
 let installInterruptHandler = installHandler sigINT (handleInterrupt [_watcherThread,_updaterThread,_reloaderThread,_mainThread])
 keepAlive$ installInterruptHandler

-- pluginThreads

handleInterrupt :: (Show a, Traversable t) => t ThreadId -> a -> IO b
handleInterrupt threadIds signal = do
 print$ "CAUGHT SIGNAL: " ++ show signal 
 _ <- traverse (\t -> throwTo t UserInterrupt) threadIds
 -- _ <- traverse killThread threadIds
 print$ "THROWING INTERRUPT"
 throw UserInterrupt

