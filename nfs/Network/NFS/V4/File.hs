-- |Operations on file handles and references.
module Network.NFS.V4.File
  ( FileHandle
  , FileName
  , FileReference(..)
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
type FileName = StrCS

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
opFileReference :: FileReference -> Ops ()
opFileReference FileRoot = op_ NFS.PUTROOTFH4args
opFileReference (FileHandle h) = op_ $ NFS.PUTFH4args h
opFileReference (FileLookup r n) = opFileReference r *> op_ (NFS.LOOKUP4args $ toOpaque' n)
opFileReference (FileParent r) = opFileReference r *> op_ NFS.LOOKUPP4args

opGetFileHandle :: Ops FileHandle
opGetFileHandle = NFS.gETFH4resok'object . NFS.gETFH4res'resok4 <$> op NFS.GETFH4args

opUpdateFileReference :: FileReference -> Ops FileReference
opUpdateFileReference FileRoot = pure FileRoot
opUpdateFileReference (FileHandle h) = pure (FileHandle h)
opUpdateFileReference _ = FileHandle <$> opGetFileHandle

-- |'opFileReference', and return a possibly-updated FH reference.
-- You should use this if the reference may involve lookups and you want to re-use the FH later.
opFileReferenceGet :: FileReference -> Ops FileReference
opFileReferenceGet f = opFileReference f *> opUpdateFileReference f
