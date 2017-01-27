{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module Network.WebDAV.NFS
  (
  ) where

import           Control.Exception (handle)
import           Control.Monad (unless)
import           Data.Bits ((.|.))
import qualified Data.Text as T
import           Data.Time.Clock.POSIX (POSIXTime)
import           Data.Word (Word64)
import qualified Network.HTTP.Types as HTTP
import qualified Network.NFS.V4 as NFS
import qualified Network.Wai as Wai

import           Network.WebDAV.NFS.Result

data NFSRoot = NFSRoot
  { nfsClient :: NFS.Client
  , nfsRoot :: NFS.FileReference
  , nfsDotFiles :: Bool
  }

errorResponse :: HTTP.Status -> Wai.Response
errorResponse s = Wai.responseLBS s [] mempty

errorResult :: HTTP.Status -> IO a
errorResult = result . errorResponse

unsafeErrorResult :: HTTP.Status -> IO a
unsafeErrorResult = unsafeResult . errorResponse

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

parsePath :: NFSRoot -> [T.Text] -> Maybe [NFS.FileName]
parsePath _ ("":_) = Nothing
parsePath _ (".":_) = Nothing
parsePath _ ("..":_) = Nothing
parsePath NFSRoot{ nfsDotFiles = False } ((T.head -> '.'):_) = Nothing
parsePath r (f:l) = (NFS.StrCS f :) <$> parsePath r l
parsePath _ [] = Just []

checkAccess :: NFS.Uint32_t -> NFS.Ops (IO ())
checkAccess a = (`unless` errorResult HTTP.forbidden403) . (a ==) . NFS.aCCESS4resok'access . NFS.aCCESS4res'resok4 <$> NFS.op (NFS.ACCESS4args a)

data FileInfo = FileInfo
  { fileType :: NFS.Nfs_ftype4
  , fileChange :: NFS.Changeid4
  , fileSize :: Word64
  , fileMTime :: POSIXTime
  }

getFileInfo :: NFS.Ops FileInfo
getFileInfo = fi . NFS.decodeAttrs . NFS.gETATTR4resok'obj_attributes . NFS.gETATTR4res'resok4
  <$> NFS.op (NFS.GETATTR4args $ NFS.encodeBitmap $
    NFS.fATTR4_TYPE .|. NFS.fATTR4_CHANGE .|. NFS.fATTR4_SIZE .|. NFS.fATTR4_TIME_MODIFY)
  where
  fi (Right [NFS.AttrValType ftyp, NFS.AttrValChange tag, NFS.AttrValSize size, NFS.AttrValTimeModify mtime]) = FileInfo ftyp tag size (NFS.decodeTime mtime)
  fi e = error $ "GETATTR: " ++ show e -- 500 error

webDAVNFS :: NFSRoot -> Wai.Application
webDAVNFS nfs = resultApplication $ \req -> do
  pathref <- maybe (errorResult HTTP.notFound404) (return . NFS.relativeFileReference (nfsRoot nfs))
    $ parsePath nfs $ Wai.pathInfo req
  case Wai.requestMethod req of
    "GET" -> do
      ((ref, access), info) <-
        handleNFSException $ NFS.nfsCall (nfsClient nfs)
          $ NFS.opFileReferenceGet pathref
          NFS.>*< checkAccess NFS.aCCESS4_READ
          NFS.>*< getFileInfo
      access
      fail "TODO"
