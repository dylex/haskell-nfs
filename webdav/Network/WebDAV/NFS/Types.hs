{-# LANGUAGE OverloadedStrings #-}
module Network.WebDAV.NFS.Types
  ( NFSRoot(..)
  , buildBS
  , emptyResponse
  , emptyResult
  , errorResponse
  , errorResult
  , handleNFSException
  ) where

import           Control.Exception (handle)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy as BSL
import           Data.Word (Word32)
import qualified Network.HTTP.Types as HTTP
import qualified Network.NFS.V4 as NFS
import qualified Network.Wai as Wai
import           Waimwork.Result (result)

data NFSRoot = NFSRoot
  { nfsClient :: NFS.Client
  , nfsRoot :: NFS.FileReference
  , nfsDotFiles :: Bool
  , nfsBlockSize :: Word32
  }

buildBS :: BSB.Builder -> BS.ByteString
buildBS = BSL.toStrict . BSB.toLazyByteString

emptyResponse :: HTTP.Status -> HTTP.ResponseHeaders -> Wai.Response
emptyResponse s h = Wai.responseLBS s h mempty

emptyResult :: HTTP.Status -> HTTP.ResponseHeaders -> IO a
emptyResult s = result . emptyResponse s

errorResponse :: HTTP.Status -> Wai.Response
errorResponse s = emptyResponse s []

errorResult :: HTTP.Status -> IO a
errorResult = result . errorResponse

nfsErrorStatus :: NFS.Nfsstat4 -> HTTP.Status
nfsErrorStatus NFS.NFS4_OK = HTTP.ok200
nfsErrorStatus NFS.NFS4ERR_PERM = HTTP.forbidden403
nfsErrorStatus NFS.NFS4ERR_ACCESS = HTTP.forbidden403
nfsErrorStatus NFS.NFS4ERR_NOENT = HTTP.notFound404
nfsErrorStatus NFS.NFS4ERR_NOTDIR = HTTP.notFound404
nfsErrorStatus NFS.NFS4ERR_EXIST = HTTP.preconditionFailed412 -- for COPY without overwrite
nfsErrorStatus NFS.NFS4ERR_NAMETOOLONG = HTTP.requestURITooLong414
nfsErrorStatus NFS.NFS4ERR_LOCKED = HTTP.mkStatus 423 "Locked"
nfsErrorStatus NFS.NFS4ERR_NOSPC = HTTP.mkStatus 507 "Insufficient Storage"
nfsErrorStatus NFS.NFS4ERR_DQUOT = HTTP.mkStatus 507 "Insufficient Storage"
nfsErrorStatus _ = HTTP.internalServerError500

handleNFSException :: IO a -> IO a
handleNFSException = handle $ errorResult . maybe HTTP.internalServerError500 nfsErrorStatus . NFS.nfsExceptionStatus

