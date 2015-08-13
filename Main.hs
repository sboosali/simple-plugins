import           SimplePlugins
import Control.Monad
import Control.Concurrent

main :: IO ()
main = do
 print "Plugins"
 forever$ do
  recompiler "Example.hs" []
  threadDelay 5000000
