{-# OPTIONS_GHC -O0 -fno-warn-missing-signatures -fno-warn-partial-type-signatures #-}
import           Example
import           SimplePlugins

main = do
 print "Plugins"
 -- recompiler proxy "./executables/Example.hs" ["./sources"]
 recompiler proxy "./executables/Example.hs" []
