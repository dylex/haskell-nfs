{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Network.WebDAV.NFS.File
  ( parsePath
  , nfsFileCall
  , fileInfoBitmap
  , decodeFileInfo
  , getFileInfo
  , checkFileInfo
  , methodNotAllowedResponse
  ) where

import           Control.Monad (guard, when, unless)
import           Data.Bits ((.&.), (.|.))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import           Data.List (foldl')
import           Data.Maybe (isNothing)
import qualified Data.Text as T
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import qualified Network.HTTP.Types as HTTP
import           Network.ONCRPC.XDR.Array (emptyBoundedLengthArray)
import qualified Network.NFS.V4 as NFS
import qualified Network.Wai as Wai
import           Waimwork.HTTP (ETag(..))

import           Network.WebDAV.NFS.Types
import           Network.WebDAV.NFS.Response

parsePath :: NFSRoot -> [T.Text] -> Maybe [NFS.FileName]
parsePath nfs ("":l) = parsePath nfs l
parsePath nfs (f:l) = guard (validFileName nfs f) >> (NFS.StrCS f :) <$> parsePath nfs l
parsePath _ [] = return []

nfsFileCall :: Context -> NFS.Ops a -> IO a
nfsFileCall ctx ops = nfsCall (contextNFS ctx)
  $ NFS.op (NFS.PUTFH4args (fileHandle (contextFile ctx))) *> ops

emptyFileInfo :: FileInfo
emptyFileInfo = FileInfo
  { fileHandle = emptyBoundedLengthArray
  , fileAccess = 0
  , fileType = Nothing
  , fileETag = WeakETag ""
  , fileSize = 0
  , fileCTime = Nothing
  , fileMTime = posixSecondsToUTCTime 0
  , fileStatus = NFS.NFS4_OK
  }

updateFileAttr :: FileInfo -> NFS.AttrVal -> FileInfo
updateFileAttr f (NFS.AttrValType        x) = f{ fileType = Just x }
updateFileAttr f (NFS.AttrValChange      x) = f{ fileETag = StrongETag $ buildBS $ BSB.word64Hex x }
updateFileAttr f (NFS.AttrValSize        x) = f{ fileSize = x }
updateFileAttr f (NFS.AttrValFilehandle  x) = f{ fileHandle = x }
updateFileAttr f (NFS.AttrValTimeCreate  x) = f{ fileCTime = Just $ posixSecondsToUTCTime $ NFS.decodeTime x }
updateFileAttr f (NFS.AttrValTimeModify  x) = f{ fileMTime = posixSecondsToUTCTime $ NFS.decodeTime x }
updateFileAttr f (NFS.AttrValRdattrError x) = f{ fileStatus = x }
updateFileAttr f _ = f

decodeFileInfo :: NFS.Fattr4 -> FileInfo
decodeFileInfo = either
  (error . ("FATTR: " ++)) -- 500 error
  (foldl' updateFileAttr emptyFileInfo)
  . NFS.decodeAttrs

updateFileAccess :: NFS.Uint32_t -> FileInfo -> FileInfo
updateFileAccess a i = i{ fileAccess = a }

fileInfoBitmap :: NFS.Bitmap
fileInfoBitmap = NFS.packBitmap
  [ NFS.AttrTypeType
  , NFS.AttrTypeChange
  , NFS.AttrTypeSize
  , NFS.AttrTypeFilehandle
  , NFS.AttrTypeTimeCreate
  , NFS.AttrTypeTimeModify
  ]

getFileInfo :: NFS.FileReference -> NFS.Ops FileInfo
getFileInfo fr = NFS.opFileReference fr
  *> (updateFileAccess . NFS.aCCESS4resok'access . NFS.aCCESS4res'resok4 <$> NFS.op
    (NFS.ACCESS4args $ NFS.aCCESS4_READ .|. NFS.aCCESS4_LOOKUP .|. NFS.aCCESS4_MODIFY .|. NFS.aCCESS4_EXTEND .|. NFS.aCCESS4_DELETE))
  <*> (decodeFileInfo . NFS.gETATTR4resok'obj_attributes . NFS.gETATTR4res'resok4 <$> NFS.op (NFS.GETATTR4args $ NFS.encodeBitmap fileInfoBitmap))

checkFileInfo :: NFS.Uint32_t -> FileInfo -> IO ()
checkFileInfo a i = do
  checkNFSStatus $ fileStatus i
  when
    (  fileHandle i == fileHandle emptyFileInfo
    || isNothing (fileType i)
    || fileETag i == fileETag emptyFileInfo)
    $ throwDAVError $ DAVStatus HTTP.internalServerError500
  unless (a .&. fileAccess i == a)
    $ throwDAVError $ DAVStatus HTTP.forbidden403

fileAcceptMethods :: FileInfo -> [HTTP.Method]
fileAcceptMethods i = ["OPTIONS", "PROPFIND"] ++ case fileType i of
  Just NFS.NF4REG -> ["GET", "HEAD"]
  _ -> []

methodNotAllowedResponse :: FileInfo -> Wai.Response
methodNotAllowedResponse i = emptyResponse HTTP.methodNotAllowed405
  [ (HTTP.hAccept, BS.intercalate "," $ fileAcceptMethods i)
  ]
