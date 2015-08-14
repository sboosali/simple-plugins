import           SimplePlugins
import           SimplePlugins.Types

import           Data.Proxy

proxy :: Proxy Plugin
proxy = Proxy

main :: IO ()
main = do
 print "Plugins"
 recompiler proxy "/Users/sambo/voice/simple-plugins/executables/Example.hs" ["/Users/sambo/voice/simple-plugins/sources","/Applications/ghc-7.10.1.app/Contents/lib/ghc-7.10.1/ghc_EMlWrQ42XY0BNVbSrKixqY"]
  -- recompiler "Example.hs" ["../sources"]
  -- recompiler "/Users/sambo/voice/simple-plugins/executables/Example.hs" ["/Users/sambo/voice/simple-plugins/sources"]
-- searches for a .{hs,lhs,hsig,lhsig} file

