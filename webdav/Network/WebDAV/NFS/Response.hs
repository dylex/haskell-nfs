{-# LANGUAGE OverloadedStrings #-}
module Network.WebDAV.NFS.Response
  ( emptyResponse
  , xmlStreamResponse
  , xmlResponse
  , DAVError(..)
  , davErrorStatus
  , davErrorElements
  , throwDAV
  , throwHTTP
  , handleDAV
  , checkNFSStatus
  , handleNFS
  ) where

import           Control.Exception (Exception, throwIO, handle)
import qualified Data.Conduit as C
import           Data.Maybe (fromMaybe)
import           Data.Typeable (Typeable)
import qualified Network.HTTP.Types as HTTP
import qualified Network.NFS.V4 as NFS
import qualified Network.Wai as Wai
import qualified Network.Wai.Conduit as WaiC

import           Network.WebDAV.XML
import           Network.WebDAV.DAV

emptyResponse :: HTTP.Status -> HTTP.ResponseHeaders -> Wai.Response
emptyResponse s h = Wai.responseLBS s h mempty

xmlStreamResponse :: HTTP.Status -> HTTP.ResponseHeaders -> XMLSource IO -> Wai.Response
xmlStreamResponse s h x = WaiC.responseSource s ((HTTP.hContentType, "application/xml; charset=\"utf-8\"") : h)
  $ C.mapOutput C.Chunk $ x C..| xmlRender

xmlResponse :: XML a => HTTP.Status -> HTTP.ResponseHeaders -> a -> Wai.Response
xmlResponse s h = xmlStreamResponse s h . xmlSource

errorResponse :: ErrorElement -> Wai.Response
errorResponse e = xmlResponse (errorElementStatus e) [] e

data DAVError
  = DAVError ErrorElement
  | HTTPError HTTP.Status HTTP.ResponseHeaders
  deriving (Typeable, Show)

instance Exception DAVError

davErrorStatus :: DAVError -> HTTP.Status
davErrorStatus (DAVError e) = errorElementStatus e
davErrorStatus (HTTPError s _) = s

davErrorElements :: DAVError -> Error
davErrorElements (DAVError e) = [Right e]
davErrorElements (HTTPError _ _) = []

davErrorResponse :: DAVError -> Wai.Response
davErrorResponse (DAVError e) = errorResponse e
davErrorResponse (HTTPError s h) = emptyResponse s h

throwDAV :: DAVError -> IO a
throwDAV = throwIO

throwHTTP :: HTTP.Status -> IO a
throwHTTP s = throwDAV $ HTTPError s []

handleDAV :: IO Wai.Response -> IO Wai.Response
handleDAV = handle $ return . davErrorResponse

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

throwNFSStatus :: NFS.Nfsstat4 -> IO a
throwNFSStatus e = throwDAV $ HTTPError (nfsErrorStatus e) []

checkNFSStatus :: NFS.Nfsstat4 -> IO ()
checkNFSStatus NFS.NFS4_OK = return ()
checkNFSStatus e = throwNFSStatus e

handleNFS :: IO a -> IO a
handleNFS = handle $ throwNFSStatus . fromMaybe NFS.NFS4ERR_BADXDR . NFS.nfsExceptionStatus
