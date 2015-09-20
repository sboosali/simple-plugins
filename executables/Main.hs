{-# LANGUAGE RecordWildCards, NamedFieldPuns    #-}
{-# OPTIONS_GHC -O0 -fno-warn-missing-signatures -fno-warn-partial-type-signatures #-}
import           Example.Plugin
-- import qualified Example.Pipes

import           SimplePlugins
import           SimplePlugins.Types 

import System.IO (hPutStrLn,stderr) 
-- import           Control.Concurrent


main = do
 recompiler pluginProxy exampleLoaderConfig exampleGhcConfig
 -- Example.Pipes.main

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
