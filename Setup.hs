import Distribution.Simple
import Network.ONCRPC.XDR.Cabal

main :: IO ()
main = defaultMainWithHooks simpleUserHooks
  { hookedPreProcessors = [ppRPCGenSuffixHandler]
  }
