{-# LANGUAGE OverloadedStrings #-}
module Network.WebDAV.NFS.Types
  ( NFSRoot(..)
  , Context(..)
  , requestHeader
  , requestDepth
  , buildBS
  , emptyResponse
  , emptyResult
  , exceptionResponse
  , exceptionResult
  , errorResponse
  , errorResult
  , handleNFSException
  ) where

import           Control.Exception (handle, Exception, displayException)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC
import           Data.Word (Word32)
import qualified Network.HTTP.Types as HTTP
import qualified Network.NFS.V4 as NFS
import qualified Network.Wai as Wai
import           Text.Read (readMaybe)
import           Waimwork.HTTP (unquoteHTTP)
import           Waimwork.Result (result)

import Network.WebDAV.DAV

data NFSRoot = NFSRoot
  { nfsClient :: NFS.Client
  , nfsRoot :: NFS.FileReference
  , nfsReadOnly :: !Bool
  , nfsDotFiles :: !Bool
  , nfsBlockSize :: !Word32
  }

data Context = Context
  { contextNFS :: !NFSRoot
  , contextRequest :: !Wai.Request
  }

requestHeader :: Context -> HTTP.HeaderName -> Maybe BS.ByteString
requestHeader c h = lookup h $ Wai.requestHeaders $ contextRequest c

requestDepth :: Context -> Maybe Depth
requestDepth c = foldMap (readMaybe . BSC.unpack . unquoteHTTP) $ requestHeader c "depth"

buildBS :: BSB.Builder -> BS.ByteString
buildBS = BSL.toStrict . BSB.toLazyByteString

exceptionResponse :: Exception e => HTTP.Status -> e -> Wai.Response
exceptionResponse s = Wai.responseLBS s [(HTTP.hContentType, "text/plain")]
  . BSLC.pack . displayException

exceptionResult :: Exception e => HTTP.Status -> e -> IO b
exceptionResult s = result . exceptionResponse s

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
nfsErrorStatus NFS.NFS4ERR_LOCKED = locked423
nfsErrorStatus NFS.NFS4ERR_NOSPC = insufficientStorage507
nfsErrorStatus NFS.NFS4ERR_DQUOT = insufficientStorage507
nfsErrorStatus _ = HTTP.internalServerError500

handleNFSException :: IO a -> IO a
handleNFSException = handle $ \e -> exceptionResult (maybe HTTP.internalServerError500 nfsErrorStatus (NFS.nfsExceptionStatus e)) e
