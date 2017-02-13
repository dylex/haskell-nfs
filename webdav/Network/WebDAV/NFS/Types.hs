{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module Network.WebDAV.NFS.Types
  ( WebDAVNFS(..)
  , FileInfo(..)
  , Context(..)
  , validFileName
  , buildBS
  , nfsCall
  ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import           Data.Time.Clock (UTCTime)
import           Data.Word (Word32, Word64)
import qualified Network.NFS.V4 as NFS
import qualified Network.Wai as Wai
import           Waimwork.HTTP (ETag)

import           Network.WebDAV.NFS.Response

data WebDAVNFS = WebDAVNFS
  { webDAVRoot :: [T.Text] -- ^HTTP prefix for serving WebDAV requests
  , nfsClient :: NFS.Client -- ^NFS client connection to use
  , nfsRoot :: NFS.FileReference -- ^NFS directory to which 'webDAVRoot' maps
  , webDAVReadOnly :: !Bool -- ^Disallow modifying access
  , webDAVDotFiles :: !Bool -- ^Allow access to hidden dot-files
  , nfsBlockSize :: !Word32 -- ^Maximum block size for read, write, readdir, etc. NFS requests 
  }

data FileInfo = FileInfo
  { filePath :: [NFS.FileName] -- ^Relative to 'webDAVRoot' and 'nfsRoot'
  , fileHandle :: NFS.FileHandle
  , fileAccess :: NFS.Uint32_t
  , fileType :: Maybe NFS.Nfs_ftype4
  , fileETag :: ETag
  , fileSize :: Word64
  , fileCTime :: Maybe UTCTime
  , fileMTime :: UTCTime
  , fileStatus :: NFS.Nfsstat4
  }

data Context = Context
  { context :: WebDAVNFS
  , contextRequest :: Wai.Request
  , contextFile :: FileInfo
  }

validFileName :: WebDAVNFS -> NFS.FileName -> Bool
validFileName _ "" = False
validFileName _ "." = False
validFileName _ ".." = False
validFileName WebDAVNFS{ webDAVDotFiles = False } (T.head . NFS.strCSText -> '.') = False
validFileName _ _ = True

buildBS :: BSB.Builder -> BS.ByteString
buildBS = BSL.toStrict . BSB.toLazyByteString

nfsCall :: WebDAVNFS -> NFS.Ops a -> IO a
nfsCall nfs = handleNFS . NFS.nfsCall (nfsClient nfs)
