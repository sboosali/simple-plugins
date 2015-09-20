{-# LANGUAGE AutoDeriveTypeable, RecordWildCards  #-}
module SimplePlugins.Types where

import DynFlags (FlushOut(..), defaultFatalMessager, defaultFlushOut)


data Plugin = Plugin String
-- data Plugin a = Plugin a
 deriving (Show,Eq,Ord)

{- |

@
@

-}
data LoaderConfig = LoaderConfig
 { _sandboxDirectory :: String   -- ^ the sandbox directory that contains @cabal.sandbox.config@
 , _pluginFile       :: String   -- ^ the root file, it's dependencies will be chased and compiled. relative to _pluginDirectory.
 , _pluginDirectory  :: String   -- ^ the directory to watch for file changes (recursively) 
 , _pluginExtensions :: [String] -- ^ the directory to watch for file changes (recursively) 
 , _sandboxPackageDB :: String   -- ^ the @package-db:@ field in @cabal.sandbox.config@
 , _inplacePackageDB :: String   -- ^ the package database used when an executable depends on a library and they live in the same package 
 , _ghcCompilationVerbosity :: Int  -- ^ @3@ is like @ghc -v@, useful for debugging
 , _ghcFatalMessager     :: String -> IO () -- ^ by default, prints error messages to 'stderr'
 , _ghcFlushOut          :: IO ()           -- ^ by default, flushes 'stdout'
 }

-- | uses relative paths. takes the platform specific _sandboxPackageDB. 
defaultLoaderConfig :: FilePath -> LoaderConfig 
defaultLoaderConfig sandboxPackageDB = LoaderConfig{..}
 where
 _sandboxDirectory = "."
 _pluginFile       = "Plugin.hs"
 _pluginDirectory  = "plugins"
 _pluginExtensions = [".hs"] 
 _inplacePackageDB = "dist/package.conf.inplace"
 _sandboxPackageDB = sandboxPackageDB
 _ghcCompilationVerbosity = 1
 _ghcFatalMessager        = defaultFatalMessager
 FlushOut _ghcFlushOut    = defaultFlushOut        -- user avoids GHC library dependency 

