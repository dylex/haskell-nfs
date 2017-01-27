-- |Operations on file handles and references.
module Network.NFS.V4.File
  ( FileReference(..)
  , relativeFileReference
  , absoluteFileReference
  , opFileReference
  , opFileReferenceGet
  ) where

import           Network.ONCRPC.XDR.Opaque (toOpaque')

import qualified Network.NFS.V4.Prot as NFS
import           Network.NFS.V4.String
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

-- |Set the current FH to a 'FileReference'.
opFileReference :: FileReference -> NFSOps ()
opFileReference FileRoot = nfsOp_ NFS.PUTROOTFH4args
opFileReference (FileHandle h) = nfsOp_ $ NFS.PUTFH4args h
opFileReference (FileLookup r n) = nfsOp_ (NFS.LOOKUP4args $ toOpaque' n) <* opFileReference r 
opFileReference (FileParent r) = nfsOp_ NFS.LOOKUPP4args <* opFileReference r

opGetFileHandle :: NFSOps FileHandle
opGetFileHandle = NFS.gETFH4resok'object . NFS.gETFH4res'resok4 <$> nfsOp NFS.GETFH4args

opUpdateFileReference :: FileReference -> NFSOps FileReference
opUpdateFileReference FileRoot = pure FileRoot
opUpdateFileReference (FileHandle h) = pure (FileHandle h)
opUpdateFileReference _ = FileHandle <$> opGetFileHandle

-- |'opFileReference', and return a possibly-updated FH reference.
-- You should use this if the reference may involve lookups and you want to re-use the FH later.
opFileReferenceGet :: FileReference -> NFSOps FileReference
opFileReferenceGet f = opUpdateFileReference f <* opFileReference f
