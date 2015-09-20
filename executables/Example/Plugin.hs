{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -O0 -fno-warn-missing-signatures -fno-warn-partial-type-signatures #-}
module Example.Plugin where
-- import Example.Extras

import           SimplePlugins.Types

import           Data.Proxy


pluginProxy :: Proxy Plugin
pluginProxy = Proxy

plugin :: Plugin
plugin = Plugin (show 'plugin) 
-- plugin = Plugin (show 'plugin ++ " reloaded") 

-- plugin = Plugin "reloaded" -- it sees the change
-- plugin =                   -- it prints the parse error
-- plugin = 0 + ""            -- it prints the type error
-- plugin = 0                 -- it prints the runtime cast (to String) error

