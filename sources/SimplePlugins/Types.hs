
module SimplePlugins.Types where
import           Data.Typeable

data Plugin = Plugin String
-- data Plugin a = Plugin a
 deriving (Show,Eq,Ord,Typeable) -- {-# LANGUAGE AutoDeriveTypeable #-}
