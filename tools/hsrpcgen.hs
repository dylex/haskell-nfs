{-# LANGUAGE RecordWildCards #-}
import           Data.List (foldl')
import           Data.Maybe (fromMaybe)
import qualified System.Console.GetOpt as Opt
import           System.Environment (getProgName, getArgs)
import           System.Exit (exitFailure)
import           System.IO (hPutStrLn, stderr)

import           Network.ONCRPC.XDR.Generate

data Opts = Opts
  { optGenerate :: GenerateOptions
  }

defOpts :: Opts
defOpts = Opts
  { optGenerate = GenerateOptions
    { generateModuleName = "Prot"
    , generateReidentOptions = defaultReidentOptions
    }
  }

setGenerate :: (GenerateOptions -> GenerateOptions) -> Opts -> Opts
setGenerate f o = o{ optGenerate = f (optGenerate o) }

setReident :: (ReidentOptions -> ReidentOptions) -> Opts -> Opts
setReident f = setGenerate $ \g -> g{ generateReidentOptions = f (generateReidentOptions g) }

opts :: [Opt.OptDescr (Opts -> Opts)]
opts =
  [ Opt.Option "m" ["module"]
      (Opt.ReqArg (\a -> setGenerate $ \g -> g{ generateModuleName = a }) "NAME")
      ("set the generated module name [" ++ generateModuleName (optGenerate defOpts) ++ "]")
  , Opt.Option "P" ["procedures-unique"]
      (Opt.NoArg (setReident $ \r -> r{ reidentJoinProcedure = Nothing }))
      "assume program version and procedure names are unique: don't add prefixes"
  , Opt.Option "F" ["fields-unique"]
      (Opt.NoArg (setReident $ \r -> r{ reidentJoinField = Nothing }))
      "assume struct and union fields are unique: don't add type prefixes"
  , Opt.Option "L" ["lowercase-prefix"]
      (Opt.OptArg (\a -> setReident $ \r -> r{ reidentLowerPrefix = fromMaybe "_" a }) "PREFIX")
      "make identifiers lower-case by prefixing them (with underscore)"
  , Opt.Option "U" ["uppercase-prefix"]
      (Opt.ReqArg (\a -> setReident $ \r -> r{ reidentLowerPrefix = a }) "PREFIX")
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
  putStrLn =<< generateFromFile optGenerate f
