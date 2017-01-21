{-# LANGUAGE RecordWildCards #-}
import           Data.List (foldl')
import           Data.Maybe (fromMaybe)
import qualified System.Console.GetOpt as Opt
import           System.Environment (getProgName, getArgs)
import           System.Exit (exitFailure)
import           System.IO (hPutStrLn, stderr)

import           Network.ONCRPC.XDR.Generate (generateFromFile)
import           Network.ONCRPC.XDR.Reident

data Opts = Opts
  { optModuleName :: String
  , optReident :: ReidentOptions
  }

defOpts :: Opts
defOpts = Opts
  { optModuleName = "Prot"
  , optReident = defaultReidentOptions
  }

opts :: [Opt.OptDescr (Opts -> Opts)]
opts =
  [ Opt.Option "m" ["module"]
      (Opt.ReqArg (\a o -> o{ optModuleName = a }) "NAME")
      ("set the generated module name [" ++ optModuleName defOpts ++ "]")
  , Opt.Option "P" ["procedures-unique"]
      (Opt.NoArg (\o -> o{ optReident = (optReident o){ reidentJoinProcedure = Nothing } }))
      "assume program version and procedure names are unique: don't add prefixes"
  , Opt.Option "F" ["fields-unique"]
      (Opt.NoArg (\o -> o{ optReident = (optReident o){ reidentJoinField = Nothing } }))
      "assume struct and union fields are unique: don't add type prefixes"
  , Opt.Option "L" ["lowercase-prefix"]
      (Opt.OptArg (\a o -> o{ optReident = (optReident o){ reidentLowerPrefix = fromMaybe "_" a } }) "PREFIX")
      "make identifiers lower-case by prefixing them (with underscore)"
  , Opt.Option "U" ["uppercase-prefix"]
      (Opt.ReqArg (\a o -> o{ optReident = (optReident o){ reidentLowerPrefix = a } }) "PREFIX")
      "make identifiers upper-case by prefixing them"
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
  putStrLn =<< generateFromFile optReident f optModuleName
