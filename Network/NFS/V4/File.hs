module Network.NFS.V4.File
  ( nfsGetFileHandle
  ) where

import           Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Vector as V
import           Network.ONCRPC.XDR.Opaque (toOpaque')
import           System.FilePath (splitDirectories, makeRelative)

import qualified Network.NFS.V4.Prot as NFS
import           Network.NFS.V4.String
import           Network.NFS.V4.Client

type FileHandle = NFS.Nfs_fh4
type FileName = NFS4CS

data FileReference
  = FileRoot
  | FileHandle !FileHandle
  | FileLookup FileReference !FileName
  | FileParent FileReference

relativeFileReference :: FileReference -> [FileName] -> FileReference
relativeFileReference = foldl FileLookup

absoluteFileReference :: [FileName] -> FileReference
absoluteFileReference = relativeFileReference FileRoot

opFileReference :: FileReference -> V.Vector NFS.Nfs_argop4
opFileReference FileRoot = V.singleton $ NFS.Nfs_argop4'OP_PUTROOTFH
opFileReference (FileHandle h) = V.singleton $ NFS.Nfs_argop4'OP_PUTFH $ NFS.PUTFH4args h
opFileReference (FileLookup r n) = opFileReference r `V.snoc` NFS.Nfs_argop4'OP_LOOKUP (NFS.LOOKUP4args $ toOpaque' n)
opFileReference (FileParent r) = opFileReference r `V.snoc` NFS.Nfs_argop4'OP_LOOKUPP

opGetFileHandle :: V.Vector NFS.Nfs_argop4
opGetFileHandle = V.singleton NFS.Nfs_argop4'OP_GETFH

nfsGetFileHandle :: Client -> FilePath -> IO FileHandle
nfsGetFileHandle client p = do
  res <- nfsCall client $ opFileReference (absoluteFileReference $ map (NFS4CS . T.pack) $ splitDirectories $ makeRelative "/" p) <> opGetFileHandle
  case V.last res of
    NFS.Nfs_resop4'OP_GETFH (NFS.GETFH4res'NFS4_OK (NFS.GETFH4resok h)) -> return h
    NFS.Nfs_resop4'OP_GETFH (NFS.GETFH4res'default stat) -> fail $ "GETFH error: " ++ show stat
    r -> fail $ "Unexpected resop: " ++ show (NFS.nfs_resop4'resop r)
