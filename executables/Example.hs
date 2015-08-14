{-# LANGUAGE TemplateHaskell #-}
import           Language.Haskell.TH.Syntax ()
import           SimplePlugins.Types


plugin = show 'plugin

-- plugin :: Plugin
-- plugin = Plugin "example"

-- plugin = Plugin "reloaded" -- it sees the change
-- plugin =            -- it prints the parse error
-- plugin = 0 + ""     -- it prints the type error
-- plugin = 0             -- it prints the runtime cast (to String) error

-- newtype (:~>:) f g = NaturalTransformation (forall x. f x -> g x) {-# LANGUAGE TypeOperators, RankNTypes #-}


