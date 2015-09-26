{-# LANGUAGE AutoDeriveTypeable, RecordWildCards, ConstraintKinds, PatternSynonyms, GeneralizedNewtypeDeriving   #-}
module SimplePlugins.Types where
import           SimplePlugins.Etc

import Data.Tagged

import Data.Typeable
import           Control.Monad.IO.Class

import DynFlags (FlushOut(..), defaultFatalMessager, defaultFlushOut)
import HscTypes (SourceError)
import GhcMonad (GhcMonad(..), Ghc, printException)
import Exception (ExceptionMonad(..)) 
import           DynFlags (HasDynFlags(..)) 
import           GHC () 


-- | an identifier tagged with the plugin type (it will be casted into, when reloaded) 
type Identifier a = Tagged a String

-- | e.g. @(Identifier "myPlugin" :: Identifier MyPlugin)@
-- (uses @PatternSynonyms@)
pattern Identifier i = Tagged i

-- | the constraints a plugin type should satisfy (uses @ConstraintKinds@)
type IsPlugin a = (Typeable a) 

{- |

-}
data LoaderConfig = LoaderConfig
 { _sandboxDirectory :: FilePath   -- ^ the sandbox directory that contains @cabal.sandbox.config@
 , _pluginFile       :: FilePath   -- ^ the root file, it's dependencies will be chased and compiled. relative to _pluginDirectory.
 , _pluginDirectory  :: FilePath   -- ^ the directory to watch for file changes (recursively) 
 , _pluginExtensions :: [String]   -- ^ the directory to watch for file changes (recursively) 
 , _sandboxPackageDB :: FilePath   -- ^ the @package-db:@ field in @cabal.sandbox.config@
 , _inplacePackageDB :: FilePath   -- ^ the package database used when an executable depends on a library and they live in the same package 
 , _extraPackageDBs  :: [FilePath] -- ^ any extra package databases you want, like stack's
 } deriving(Show,Eq,Ord)

-- | @(GhcMonad m)@
data GhcConfig m = GhcConfig 
 { _ghcCompilationVerbosity :: Int  -- ^ @3@ is like @ghc -v@, useful for debugging
 , _ghcFatalMessager     :: String -> IO () -- ^ by default, prints error messages to 'stderr'
 , _ghcFlushOut          :: IO ()           -- ^ by default, flushes 'stdout'
 , _ghcPrintSourceError  :: SourceError -> m () -- ^ 
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
 _extraPackageDBs  = [] 

defaultGhcConfig :: (GhcMonad m) => GhcConfig m
defaultGhcConfig = GhcConfig{..}
 where
 _ghcCompilationVerbosity = 1
 _ghcFatalMessager        = defaultFatalMessager
 FlushOut _ghcFlushOut    = defaultFlushOut        -- user avoids GHC library dependency 
 _ghcPrintSourceError     = printException

-- | a safer 'GhcMonad'. it's usage should try to catch as many exceptions, at the right places, as it can. 
newtype SaferGhc a = SaferGhc { runSaferGhc :: Ghc a }
 deriving
  ( Functor, Applicative, Monad
  , MonadIO, GhcMonad, HasDynFlags
  -- , ExceptionMonad 
  )

instance ExceptionMonad SaferGhc where 
 gcatch (SaferGhc m) handler = SaferGhc$ gcatch m (runSaferGhc . handler) 
 gmask restore = SaferGhc$ do 
  liftIO$ print$ s"MASKING"
  gmask (\action -> (runSaferGhc . restore) (SaferGhc . action . runSaferGhc)) 
