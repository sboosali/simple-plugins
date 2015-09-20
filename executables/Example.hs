{-# OPTIONS_GHC -O0 -fno-warn-missing-signatures -fno-warn-partial-type-signatures #-}
module Example where
import           SimplePlugins.Types

import           Data.Proxy

proxy :: Proxy Plugin
proxy = Proxy

plugin :: Plugin
plugin = Plugin "plugin"

-- plugin = Plugin "reloaded" -- it sees the change
-- plugin =                   -- it prints the parse error
-- plugin = 0 + ""            -- it prints the type error
-- plugin = 0                 -- it prints the runtime cast (to String) error

