{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
import           Data.List (foldl', intercalate)
import           Data.Maybe (fromMaybe)
import           Data.String (fromString)
import           Data.Word (Word32)
import qualified Network.NFS.V4 as NFS
import qualified Network.Wai.Handler.Warp as Warp (run)
import qualified System.Console.GetOpt as Opt
import           System.Environment (getProgName, getArgs)
import           System.Exit (exitFailure)
import           System.FilePath.Posix (splitDirectories)
import           System.IO (hPutStrLn, stderr)

import           Network.WebDAV.NFS

data Opts = Opts
  { optServer :: String
  , optRoot :: FilePath
  , optPort :: Int
  , optDotFiles :: Bool
  , optBlockSize :: Word32
  }

defOpts :: Opts
defOpts = Opts
  { optServer = "localhost"
  , optRoot = "/"
  , optPort = 8049
  , optDotFiles = False
  , optBlockSize = 131072
  }

opts :: [Opt.OptDescr (Opts -> Opts)]
opts =
  [ Opt.Option "d" ["dot-files"]
      (Opt.NoArg (\o -> o{ optDotFiles = True }))
      "allow access to dot-files"
  , Opt.Option "d" ["dot-files"]
      (Opt.NoArg (\o -> o{ optDotFiles = True }))
      "allow access to dot-files"
  , Opt.Option "b" ["block-size"]
      (Opt.ReqArg (\a o -> o{ optBlockSize = read a }) "BYTES")
      ("NFS read/write block size [" ++ show (optBlockSize defOpts) ++ "]")
  ]

main :: IO ()
main = do
  prog <- getProgName
  args <- getArgs
  Opts{..} <- case Opt.getOpt Opt.Permute opts args of
    (ol, [break (':' ==) -> (h, ':':'/':p)], []) ->
      return $ foldl' (flip ($)) defOpts{ optServer = h, optRoot = p } ol
    (_, _, err) -> do
      mapM_ (hPutStrLn stderr) err
      hPutStrLn stderr $ Opt.usageInfo ("Usage: " ++ prog ++ " [OPTION...] NFSSERVER:/ROOT") opts
      exitFailure
  client <- NFS.openClient optServer
  Warp.run optPort $ webDAVNFS NFSRoot
    { nfsClient = client
    , nfsRoot = NFS.absoluteFileReference $ map fromString $ splitDirectories optRoot
    , nfsDotFiles = optDotFiles
    , nfsBlockSize = optBlockSize
    }
