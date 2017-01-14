
import           Data.List (foldl')
import           Language.Haskell.Exts.Pretty (prettyPrint)
import qualified System.Console.GetOpt as Opt
import           System.Environment (getProgName, getArgs)
import           System.Exit (exitFailure)
import           System.IO (hPutStrLn, stderr)

import qualified Data.XDR.Parse as XDR
import qualified Data.XDR.Generate as XDR

data Opts = Opts
  { 
  }

defOpts :: Opts
defOpts = Opts
  { 
  }

opts :: [Opt.OptDescr (Opts -> Opts)]
opts =
  [ 
  ]

main :: IO ()
main = do
  prog <- getProgName
  args <- getArgs
  (_, f) <- case Opt.getOpt Opt.Permute opts args of
    (ol, [f], []) -> return (foldl' (flip ($)) defOpts ol, f)
    (_, _, err) -> do
      mapM_ (hPutStrLn stderr) err
      hPutStrLn stderr $ Opt.usageInfo ("Usage: " ++ prog ++ " [OPTION...] FILE") opts
      exitFailure
  (d, s) <- either
    (\e -> do
      hPutStrLn stderr $ show e
      exitFailure)
    return
    =<< XDR.parseFile f
  putStrLn $ Language.Haskell.Exts.Pretty.prettyPrint $ XDR.generate "XDR" s d
