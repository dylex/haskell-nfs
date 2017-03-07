{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
import           Data.List (foldl')
import           Data.String (fromString)
import           Data.Word (Word32)
import qualified Network.NFS.V4 as NFS
import qualified Network.ONCRPC as RPC
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
  , optReadOnly :: Bool
  , optDotFiles :: Bool
  , optBlockSize :: Word32
  , optAuthUnix :: Maybe [Word32]
  }

defOpts :: Opts
defOpts = Opts
  { optServer = "localhost"
  , optRoot = "/"
  , optPort = 8049
  , optReadOnly = False
  , optDotFiles = False
  , optBlockSize = 131072
  , optAuthUnix = Nothing
  }

opts :: [Opt.OptDescr (Opts -> Opts)]
opts =
  [ Opt.Option "p" ["port"]
      (Opt.ReqArg (\a o -> o{ optPort = read a }) "PORT")
      ("listen on port [" ++ show (optPort defOpts) ++ "]")
  , Opt.Option "r" ["read-only"]
      (Opt.NoArg (\o -> o{ optReadOnly = True }))
      "only allow non-modifying access"
  , Opt.Option "d" ["dot-files"]
      (Opt.NoArg (\o -> o{ optDotFiles = True }))
      "allow access to dot-files"
  , Opt.Option "b" ["block-size"]
      (Opt.ReqArg (\a o -> o{ optBlockSize = read a }) "BYTES")
      ("NFS read/write block size [" ++ show (optBlockSize defOpts) ++ "]")
  , Opt.Option "u" ["unix-auth"]
      (Opt.OptArg (\a o -> o{ optAuthUnix = Just $ maybe [] (map read . split) a }) "UID[:GID,...]")
      "use the specified (or your current) UNIX credentials for authentication"
  ] where
  delim ':' = True
  delim ',' = True
  delim _ = False
  split "" = []
  split (c:s) | delim c = split s
  split s = h : split r where (h,r) = break delim s

setAuthUnix :: [Word32] -> RPC.Auth -> RPC.Auth
setAuthUnix (u:gs) (RPC.AuthSys ap) = RPC.AuthSys $ setgs gs ap{ RPC.authsys_parms'uid = u } where
  setgs [] p = p
  setgs (g:gl) p = setgl gl p{ RPC.authsys_parms'gid = g }
  setgl gl p = p{ RPC.authsys_parms'gids = RPC.boundLengthArrayFromList gl }
setAuthUnix _ a = a

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
  auth <- maybe (return RPC.AuthNone)
    (\u -> setAuthUnix u <$> RPC.getAuthUnix)
    optAuthUnix
  client <- NFS.setClientAuth auth RPC.AuthNone <$> NFS.openClient optServer
  root <- NFS.nfsCall client $ NFS.opFileReferenceGet $ NFS.absoluteFileReference $ map fromString $ splitDirectories optRoot
  Warp.run optPort $ webDAVNFSApplication WebDAVNFS
    { nfsClient = client
    , nfsRoot = root
    , webDAVRoot = []
    , webDAVReadOnly = optReadOnly
    , webDAVDotFiles = optDotFiles
    , nfsBlockSize = optBlockSize
    }
