{-# LANGUAGE TemplateHaskell #-}
module Example where
import           SimplePlugins.Types

import           Data.Proxy
import           Language.Haskell.TH.Syntax ()

proxy :: Proxy Plugin
proxy = Proxy

plugin :: Plugin
plugin = Plugin (show 'plugin)

-- plugin = Plugin "reloaded" -- it sees the change
-- plugin =            -- it prints the parse error
-- plugin = 0 + ""     -- it prints the type error
-- plugin = 0             -- it prints the runtime cast (to String) error

-- newtype (:~>:) f g = NaturalTransformation (forall x. f x -> g x) {-# LANGUAGE TypeOperators, RankNTypes #-}


