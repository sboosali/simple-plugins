{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -O0 -fno-warn-missing-signatures -fno-warn-partial-type-signatures #-}
module Example.Plugin where
-- import Example.Extras

plugin = (show 'plugin)         -- TemplateHaskell works 
-- plugin = (show 'plugin ++ " reloaded") 

-- plugin =                   -- it prints the parse error
-- plugin = 0 + ""            -- it prints the type error
-- plugin = 0                 -- it prints the runtime cast (to String) error
-- plugin = "reloaded" -- it sees the change
