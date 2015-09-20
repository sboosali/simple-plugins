{-# LANGUAGE RecordWildCards, NamedFieldPuns    #-}
{-# OPTIONS_GHC -O0 -fno-warn-missing-signatures -fno-warn-partial-type-signatures #-}
import           Example
import           SimplePlugins
import           SimplePlugins.Types 

import System.IO (hPutStrLn,stderr) 


main = do
 print "Plugins"
 -- recompiler proxy "./executables/Example.hs" ["./sources"]
 recompiler proxy exampleLoaderConfig exampleGhcConfig

exampleLoaderConfig = (defaultLoaderConfig sandboxPackageDB){_pluginFile, _pluginDirectory}
 where
 sandboxPackageDB = ".cabal-sandbox/x86_64-osx-ghc-7.10.1-packages.conf.d"
 _pluginFile      = "Example.hs"
 _pluginDirectory = "executables"

exampleGhcConfig = defaultGhcConfig{_ghcFatalMessager}
 where 
 _ghcFatalMessager message = do -- our message redirection works
  hPutStrLn stderr message 
  hPutStrLn stderr message 
