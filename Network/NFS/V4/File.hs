module Network.NFS.V4.File
  ( nfsGetFileHandle
  ) where

import qualified Data.Text as T
import           Network.ONCRPC.XDR.Opaque (toOpaque')
import           System.FilePath (splitDirectories, makeRelative)

import qualified Network.NFS.V4.Prot as NFS
import           Network.NFS.V4.String
import           Network.NFS.V4.Client
import           Network.NFS.V4.Ops as NFS

type FileHandle = NFS.Nfs_fh4
type FileName = NFSStrCS

data FileReference
  = FileRoot
  | FileHandle !FileHandle
  | FileLookup FileReference !FileName
  | FileParent FileReference

relativeFileReference :: FileReference -> [FileName] -> FileReference
relativeFileReference = foldl FileLookup

absoluteFileReference :: [FileName] -> FileReference
absoluteFileReference = relativeFileReference FileRoot

opFileReference :: FileReference -> NFSOps ()
opFileReference FileRoot = nfsOp_ NFS.PUTROOTFH4args
opFileReference (FileHandle h) = nfsOp_ $ NFS.PUTFH4args h
opFileReference (FileLookup r n) = nfsOp_ (NFS.LOOKUP4args $ toOpaque' n) <* opFileReference r 
opFileReference (FileParent r) = nfsOp_ NFS.LOOKUPP4args <* opFileReference r

opGetFileHandle :: NFSOps FileHandle
opGetFileHandle = nfsOp NFS.GETFH4args $ NFS.gETFH4resok'object . NFS.gETFH4res'resok4

nfsGetFileHandle :: Client -> FilePath -> IO FileHandle
nfsGetFileHandle client p =
  nfsOpCall client $ opGetFileHandle <* opFileReference (absoluteFileReference $ map (NFSStrCS . T.pack) $ splitDirectories $ makeRelative "/" p) 
