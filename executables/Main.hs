{-# LANGUAGE RecordWildCards, NamedFieldPuns    #-}
{-# OPTIONS_GHC -O0 -fno-warn-missing-signatures -fno-warn-partial-type-signatures #-}
import           Example.Plugin

import           SimplePlugins
import           SimplePlugins.Types 
import           SimplePlugins.Etc

import System.IO (hPutStrLn,stderr) 
import Control.Concurrent
import           Control.Monad
import           Control.Monad.IO.Class


-- TODO ignores user interrupt http://neilmitchell.blogspot.com/2015/05/handling-control-c-in-haskell.html
-- C-c causes "example: user interrupt" not termination 
main = do

 watcherChannel <- newChan
 pluginChannel <- newChan

 _ <- forkIO$ directoryWatcher watcherChannel exampleLoaderConfig

 _ <- forkIO$ forever$ do
   event <- readChan watcherChannel  -- blocks on reading from the channel
   print event
   withGHCSession exampleLoaderConfig exampleGhcConfig $ do
    recompileTargets pluginProxy pluginChannel exampleGhcConfig -- event

 _ <- forkIO$ forever$ do
  readChan pluginChannel >>= reloadPlugin

 forever$ threadDelay 10000000

reloadPlugin :: (Show plugin) => Maybe plugin -> IO ()
reloadPlugin plugin_ = do 
  replicateM_ 5 (putStrLn"")
  putStrLn$ "-------------------------------------------------------------------------"
  putStrLn$ "reloading plugin..."
  case plugin_ of 
      Nothing -> liftIO$ do
       putStrLn$ s"failure in plugin reloading: plugin has wrong type"
      Just plugin -> liftIO$ do
       putStrLn$ s"success in plugin reloading"
       print plugin 

exampleLoaderConfig = (defaultLoaderConfig sandboxPackageDB){_pluginFile, _pluginDirectory}
 where
 sandboxPackageDB = ".cabal-sandbox/x86_64-osx-ghc-7.10.1-packages.conf.d"
 _pluginFile      = "Example/Plugin.hs"
 _pluginDirectory = "executables"

exampleGhcConfig = defaultGhcConfig{_ghcFatalMessager}
 where 
 _ghcFatalMessager message = do -- our message redirection works
  hPutStrLn stderr message 
  hPutStrLn stderr message 

       -- print (plugin::Plugin)
