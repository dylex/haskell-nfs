{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module Network.WebDAV.NFS.Types
  ( NFSRoot(..)
  , FileInfo(..)
  , Context(..)
  , validFileName
  , subContext
  , buildBS
  , nfsCall
  ) where

import           Control.Monad (guard)
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

data NFSRoot = NFSRoot
  { nfsClient :: NFS.Client
  , nfsRoot :: NFS.FileReference
  , nfsReadOnly :: !Bool
  , nfsDotFiles :: !Bool
  , nfsBlockSize :: !Word32
  }

data FileInfo = FileInfo
  { fileHandle :: NFS.FileHandle
  , fileAccess :: NFS.Uint32_t
  , fileType :: Maybe NFS.Nfs_ftype4
  , fileETag :: ETag
  , fileSize :: Word64
  , fileCTime :: Maybe UTCTime
  , fileMTime :: UTCTime
  , fileStatus :: NFS.Nfsstat4
  }

data Context = Context
  { contextNFS :: !NFSRoot
  , contextRequest :: !Wai.Request
  , contextFile :: FileInfo
  }

validFileName :: NFSRoot -> T.Text -> Bool
validFileName _ "" = False
validFileName _ "." = False
validFileName _ ".." = False
validFileName NFSRoot{ nfsDotFiles = False } (T.head -> '.') = False
validFileName _ _ = True

subContext :: Context -> FileInfo -> NFS.FileName -> Maybe Context
subContext ctx info name = ctx
  { contextRequest = (contextRequest ctx){ Wai.pathInfo = Wai.pathInfo (contextRequest ctx) ++ [tname] }
  -- TODO: fix root: raw path info?
  , contextFile = info
  } <$ guard (validFileName (contextNFS ctx) tname)
  where
  tname = NFS.strCSText name

buildBS :: BSB.Builder -> BS.ByteString
buildBS = BSL.toStrict . BSB.toLazyByteString

nfsCall :: NFSRoot -> NFS.Ops a -> IO a
nfsCall nfs = handleNFSException . NFS.nfsCall (nfsClient nfs)
