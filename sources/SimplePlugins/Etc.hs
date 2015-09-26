module SimplePlugins.Etc where

import System.Signal 

import           Data.Typeable
import           Control.Monad
import           Control.Monad.IO.Class
import Control.Concurrent
import Control.Exception 
import Data.IORef (newIORef)

import qualified           GHC
import qualified           Fingerprint
import qualified           Outputable
import qualified StaticFlags
import qualified SysTools
import qualified DynFlags
import qualified HscMain
import qualified Panic
import qualified GhcMonad


either2maybe :: Either e a -> Maybe a 
either2maybe = either (const Nothing) Just 

-- for OverloadedStrings
s :: String -> String
s = id

showTypeRep :: TypeRep -> (String, String, String, Fingerprint.Fingerprint)
showTypeRep tr = (tyConName tc, tyConModule tc, tyConPackage tc, tyConFingerprint tc)
 where tc = typeRepTyCon tr

-- reloadPlugin :: Show a => Plugin a -> IO ()
-- reloadPlugin :: proxy plugin -> Plugin a -> IO ()
-- reloadPlugin _ (Plugin value) = do
--  print value

-- a helper from interactive-diagrams to print out GHC API values,
-- useful while debugging the API.
-- | Outputs any value that can be pretty-printed using the default style
output :: (GHC.GhcMonad m, MonadIO m) => Outputable.Outputable a => a -> m ()
output a = do
    dfs <- GHC.getSessionDynFlags
    let style = Outputable.defaultUserStyle
    let cntx  = Outputable.initSDocContext dfs style
    liftIO $ print $ Outputable.runSDoc (Outputable.ppr a) cntx

keepAlive :: IO b -> IO a
keepAlive action = forever$ do
 _ <- action 
 threadDelay (1*1000*1000) 

{- | re-implement 'runGhc' without installing signal handlers. 

the default 'installSignalHandlers' snuffs out user interrupt, when an action that runs a 'Ghc' is forked. 

(they have already been installed by the Haskell runtime) 

-}
runGhc' :: Maybe FilePath  -- ^ See argument to 'initGhcMonad'.
       -> GhcMonad.Ghc a           -- ^ The action to perform.
       -> IO a
runGhc' mb_top_dir ghc = do
  ref <- newIORef (Panic.panic "empty session")
  let session = GhcMonad.Session ref
  flip GhcMonad.unGhc session $ do
    initGhcMonad' mb_top_dir
    ghc

{- | re-implement 'initGhcMonad' without installing signal handlers 

-}
initGhcMonad' :: GHC.GhcMonad m => Maybe FilePath -> m ()
initGhcMonad' mb_top_dir
  = do { env <- liftIO $
                do { installSignalHandlers'
                   ; StaticFlags.initStaticOpts
                   ; mySettings <- SysTools.initSysTools mb_top_dir
                   ; dflags <- DynFlags.initDynFlags (DynFlags.defaultDynFlags mySettings)
                   --TODO ; GHC.checkBrokenTablesNextToCode dflags
                   ; DynFlags.setUnsafeGlobalDynFlags dflags
                      -- c.f. DynFlags.parseDynamicFlagsFull, which
                      -- creates DynFlags and sets the UnsafeGlobalDynFlags
                   ; HscMain.newHscEnv dflags }
       ; GHC.setSession env }


{- | re-implement 'installSignalHandlers' by simply throwing a UserInterrupt 

-}
installSignalHandlers' :: IO () 
installSignalHandlers' = installHandler sigINT (\_ -> throw UserInterrupt)

