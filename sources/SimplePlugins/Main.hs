{-# LANGUAGE RecordWildCards, NamedFieldPuns    #-}
{-# OPTIONS_GHC -O0 -fno-warn-missing-signatures -fno-warn-partial-type-signatures #-}
module SimplePlugins.Main where 

import           SimplePlugins
import           SimplePlugins.Types 
import           SimplePlugins.Etc

-- import qualified SlaveThread as Slave 
-- import System.Signal
-- import           System.FSNotify

import Control.Concurrent
import           Control.Monad
import           Control.Monad.IO.Class
-- import Control.Exception
-- import Control.Exception (throw) 
-- import Control.Exception (getMaskingState) 
-- import System.IO (hPutStrLn,stderr) 


-- TODO ignores user interrupt http://neilmitchell.blogspot.com/2015/05/handling-control-c-in-haskell.html
-- C-c causes "example: user interrupt" not termination 
main = do

 let fork = forkIO              --  Slave.fork

 watcherChannel <- newChan
 pluginChannel <- newChan

 _mainThread <- myThreadId
 -- _ <- installHandler sigINT (\_ -> throwTo _mainThread UserInterrupt)

 _watcherThread  <- fork$ directoryWatcher watcherChannel exampleLoaderConfig

 _updaterThread  <- fork$ pluginUpdater pluginChannel exampleUpdatePlugin

 _reloaderThread <- fork$ pluginWatcher watcherChannel pluginChannel exampleLoaderConfig exampleGhcConfig exampleIdentifier

 -- pluginReloader watcherChannel pluginChannel exampleLoaderConfig exampleGhcConfig exampleIdentifier
 keepAlive$ return() 

-- pluginThreads

-- | 
exampleUpdatePlugin :: (Show plugin) => Maybe plugin -> IO ()
exampleUpdatePlugin p_ = do 
  replicateM_ 5 (putStrLn"")
  -- putStrLn$ "-------------------------------------------------------------------------"
  -- putStrLn$ "reloading plugin..."
  case p_ of 
      Nothing -> liftIO$ do
       putStrLn$ s"failure in plugin reloading: plugin has wrong type"
      Just p -> liftIO$ do
       putStrLn$ s"success in plugin reloading"
       print p 

exampleLoaderConfig = (defaultLoaderConfig sandboxPackageDB){_pluginFile, _pluginDirectory}
 where
 sandboxPackageDB = ".cabal-sandbox/x86_64-osx-ghc-7.10.1-packages.conf.d"
 _pluginFile      = "Example/Plugin.hs"
 _pluginDirectory = "executables"

exampleGhcConfig = defaultGhcConfig

-- exampleGhcConfig = defaultGhcConfig{_ghcFatalMessager}
--  where 
--  _ghcFatalMessager _message = do -- our message redirection works
--   -- hPutStrLn stderr _message 
--   return () 
--   -- we're running GHC from a non-main thread. we should rethrow asynchronous exceptions.  

exampleIdentifier = Identifier "plugin" :: Identifier String

