{-# LANGUAGE TemplateHaskell, AutoDeriveTypeable, Rank2Types   #-}
{-# OPTIONS_GHC -O0 -fno-warn-missing-signatures -fno-warn-partial-type-signatures #-}
module Example.Plugin where
-- import           SimplePlugins.Types -- TODO dependency analysis should not chase this, or at least must not reload it

-- import Example.Extras


-- type is inferred 
-- plugin = (show 'plugin)         -- TemplateHaskell works 
plugin = (show 'plugin ++ " reloaded") 

-- plugin =                   -- it prints the parse error
-- plugin = 0 + ""            -- it prints the type error
-- plugin = 0                 -- it prints the runtime cast (to String) error
-- plugin = "reloaded" -- it sees the change


data IdentityFunction = IdentityFunction (forall a. a -> a)

-- TODO Segmentation fault: 11
function = IdentityFunction id  -- monomorphize

