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


main = do
 pluginChannel <- newChan
 _ <- forkIO$ recompiler pluginProxy pluginChannel exampleLoaderConfig exampleGhcConfig
 forever$ do
  plugin_ <- readChan pluginChannel
  reloadPlugin plugin_

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
