{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
module Network.WebDAV.NFS
  (
  ) where

import           Control.Exception (handle)
import           Control.Monad (when, unless, guard)
import           Data.Bits ((.|.))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import           Data.Maybe (mapMaybe)
import           Data.Monoid ((<>))
import qualified Data.Text as T
import           Data.Time.Clock (UTCTime)
import           Data.Time.Clock.POSIX (POSIXTime, posixSecondsToUTCTime)
import           Data.Word (Word32, Word64)
import qualified Network.HTTP.Types as HTTP
import qualified Network.HTTP.Types.Header as HTTP
import qualified Network.NFS.V4 as NFS
import qualified Network.Wai as Wai

import           Waimwork.Result (result, unsafeResult, resultApplication)
import           Waimwork.HTTP (splitHTTP, unquoteHTTP, quoteHTTP, parseHTTPDate, formatHTTPDate)

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
  , fileETag :: BS.ByteString
  , fileSize :: Word64
  , fileMTime :: UTCTime
  }

getFileInfo :: NFS.Ops FileInfo
getFileInfo = fi . NFS.decodeAttrs . NFS.gETATTR4resok'obj_attributes . NFS.gETATTR4res'resok4
  <$> NFS.op (NFS.GETATTR4args $ NFS.encodeBitmap $
    NFS.fATTR4_TYPE .|. NFS.fATTR4_CHANGE .|. NFS.fATTR4_SIZE .|. NFS.fATTR4_TIME_MODIFY)
  where
  fi (Right [NFS.AttrValType ftyp, NFS.AttrValChange tag, NFS.AttrValSize size, NFS.AttrValTimeModify mtime]) =
    FileInfo ftyp (buildBS $ BSB.word64Hex tag) size (posixSecondsToUTCTime $ NFS.decodeTime mtime)
  fi e = error $ "GETATTR: " ++ show e -- 500 error

streamFile :: NFSRoot -> NFS.FileReference -> Word64 -> Word64 -> Wai.StreamingBody
streamFile nfs ref start end send done = do
  NFS.READ4res'NFS4_OK (NFS.READ4resok eof lbuf) <- NFS.nfsCall (nfsClient nfs)
    $ NFS.op (NFS.READ4args NFS.anonymousStateid start $ fromIntegral l)
  let buf = NFS.unLengthArray lbuf
  send $ BSB.byteString buf
  let next = start + fromIntegral (BS.length buf)
  if next >= end || eof
    then done
    else streamFile nfs ref next end send done
  where
  r = end - start
  l = r `min` fromIntegral (nfsBlockSize nfs)

doGet :: NFSRoot -> Wai.Request -> NFS.FileReference -> IO Wai.Response
doGet nfs req pathref = do
  ((ref, access), FileInfo{..}) <-
    handleNFSException $ NFS.nfsCall (nfsClient nfs)
      $ NFS.opFileReferenceGet pathref
      NFS.>*< checkAccess NFS.aCCESS4_READ
      NFS.>*< getFileInfo
  access
  when (fileType /= NFS.NF4REG) $
    errorResult HTTP.methodNotAllowed405
  let headers =
        [ (HTTP.hETag, quoteHTTP fileETag)
        , (HTTP.hLastModified, formatHTTPDate fileMTime)
        , (HTTP.hAcceptRanges, "bytes")
        ]
      ismat   = all (isent fileETag) ifmat
      isnomat = all (not . isent fileETag) ifnomat
      ismod   = all (fileMTime >) ifmod
      isnomod = all (fileMTime <=) ifnomod
      isrange = all (either (fileETag ==) (fileMTime <=)) ifrange
      ranges' = guard isrange >> mapMaybe (checkr . clampr (toInteger fileSize)) <$> ranges
      sizeb = BSB.word64Dec fileSize
  unless (isnomat || ismod) $
    emptyResult HTTP.notModified304 headers
  unless (ismat || isnomod) $
    emptyResult HTTP.preconditionFailed412 headers
  return $ case ranges' of
    Nothing -> Wai.responseStream HTTP.ok200
      ((HTTP.hContentLength, buildBS sizeb) : headers)
      (streamFile nfs ref 0 fileSize)
    Just [] -> emptyResponse HTTP.requestedRangeNotSatisfiable416
      $ (HTTP.hContentRange, buildBS $ "bytes */" <> sizeb) : headers
    Just [(a,b)] -> Wai.responseStream HTTP.partialContent206
      ( (HTTP.hContentLength, buildBS $ BSB.word64Dec (succ b - a))
      : (HTTP.hContentRange, buildBS $ "bytes " <> BSB.word64Dec a <> BSB.char8 '-' <> BSB.word64Dec b <> BSB.char8 '/' <> sizeb)
      : headers)
      (streamFile nfs ref a $ succ b)
    Just rl -> errorResponse HTTP.notImplemented501 -- "multipart/byteranges"
  where
  ifmat   = splitHTTP     =<< header HTTP.hIfMatch
  ifnomat = splitHTTP     =<< header HTTP.hIfNoneMatch
  ifmod   = parseHTTPDate =<< header HTTP.hIfModifiedSince
  ifnomod = parseHTTPDate =<< header HTTP.hIfUnmodifiedSince
  ifrange = (\s -> maybe (Left $ unquoteHTTP s) Right $ parseHTTPDate s) <$> header HTTP.hIfRange
  ranges  = HTTP.parseByteRanges =<< header HTTP.hRange
  header h = lookup h $ Wai.requestHeaders req
  isent _ ["*"] = True
  isent e l = e `elem` l
  clampr z (HTTP.ByteRangeFrom a) = (a `max` 0, pred z)
  clampr z (HTTP.ByteRangeFromTo a b) = (a `max` 0, b `min` pred z)
  clampr z (HTTP.ByteRangeSuffix e) = (z - e `max` 0, pred z)
  checkr (a, b)
    | a <= b = Just (fromInteger a, fromInteger b)
    | otherwise = Nothing

webDAVNFS :: NFSRoot -> Wai.Application
webDAVNFS nfs = resultApplication $ \req -> do
  pathref <- maybe (errorResult HTTP.notFound404) (return . NFS.relativeFileReference (nfsRoot nfs))
    $ parsePath nfs $ Wai.pathInfo req
  case Wai.requestMethod req of
    "GET" -> doGet nfs req pathref
