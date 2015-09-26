{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -O0 -fno-warn-missing-signatures -fno-warn-partial-type-signatures #-}
module Main where 
import Example.Plugin

import SimplePlugins.Etc
import SimplePlugins.Types
import SimplePlugins.Main

import Control.Monad
import Control.Monad.IO.Class


main = exampleReloader 

exampleReloader :: PluginReloader 
exampleReloader = launchReloader exampleLoaderConfig exampleGhcConfig exampleUpdatePlugin exampleIdentifier

-- | 
exampleUpdatePlugin :: (Show plugin) => UpdatePlugin plugin
exampleUpdatePlugin = \p_ -> do 
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
-- exampleIdentifier = Identifier (globalName 'plugin) :: Identifier String
-- globalName :: Name -> String

exampleUpdatePlugin2  :: Maybe IdentityFunction  -> IO ()
exampleUpdatePlugin2 p_  = do 
  replicateM_ 5 (putStrLn"")
  -- putStrLn$ "-------------------------------------------------------------------------"
  -- putStrLn$ "reloading plugin..."
  case p_ of 
      Nothing -> liftIO$ do
       putStrLn$ s"failure in plugin reloading: plugin has wrong type"
      Just (IdentityFunction p) -> liftIO$ do
       putStrLn$ s"success in plugin reloading"
       print$ p True

exampleIdentifier2 = Identifier "function" :: Identifier IdentityFunction 

-- when the plug-in is polymorphic, like (a -> (a,a)), we get an ambiguity or :
-- No instance for (Data.Typeable.Internal.Typeable a0)

