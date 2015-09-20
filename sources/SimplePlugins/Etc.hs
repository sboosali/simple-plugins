module SimplePlugins.Etc where

import           Data.Typeable

import           Control.Monad.IO.Class

import           GHC
import           Fingerprint
import           Outputable


-- for OverloadedStrings
s :: String -> String
s = id

showTypeRep :: TypeRep -> (String, String, String, Fingerprint)
showTypeRep tr = (tyConName tc, tyConModule tc, tyConPackage tc, tyConFingerprint tc)
 where tc = typeRepTyCon tr

-- reloadPlugin :: Show a => Plugin a -> IO ()
-- reloadPlugin :: proxy plugin -> Plugin a -> IO ()
-- reloadPlugin _ (Plugin value) = do
--  print value

-- a helper from interactive-diagrams to print out GHC API values,
-- useful while debugging the API.
-- | Outputs any value that can be pretty-printed using the default style
output :: (GhcMonad m, MonadIO m) => Outputable a => a -> m ()
output a = do
    dfs <- getSessionDynFlags
    let style = defaultUserStyle
    let cntx  = initSDocContext dfs style
    liftIO $ print $ runSDoc (ppr a) cntx
