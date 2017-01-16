{-# LANGUAGE RecordWildCards #-}
import           Data.List (foldl')
import qualified System.Console.GetOpt as Opt
import           System.Environment (getProgName, getArgs)
import           System.Exit (exitFailure)
import           System.IO (hPutStrLn, stderr)

import           Data.XDR.Generate (generateFromFile)

data Opts = Opts
  { optModuleName :: String
  }

defOpts :: Opts
defOpts = Opts
  { optModuleName = "Prot"
  }

opts :: [Opt.OptDescr (Opts -> Opts)]
opts =
  [ Opt.Option "m" ["module"]
      (Opt.ReqArg (\a o -> o{ optModuleName = a }) "NAME")
      ("set the generated module name [" ++ optModuleName defOpts ++ "]")
  ]

main :: IO ()
main = do
  prog <- getProgName
  args <- getArgs
  (Opts{..}, f) <- case Opt.getOpt Opt.Permute opts args of
    (ol, [f], []) -> return (foldl' (flip ($)) defOpts ol, f)
    (_, _, err) -> do
      mapM_ (hPutStrLn stderr) err
      hPutStrLn stderr $ Opt.usageInfo ("Usage: " ++ prog ++ " [OPTION...] FILE") opts
      exitFailure
  putStrLn =<< generateFromFile f optModuleName
