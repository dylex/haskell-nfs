{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Network.WebDAV.NFS.GET
  ( httpGET
  ) where

import           Control.Monad (when, unless, guard)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import           Data.Maybe (mapMaybe)
import           Data.Monoid ((<>))
import           Data.Word (Word64)
import qualified Network.HTTP.Types as HTTP
import qualified Network.HTTP.Types.Header as HTTP
import qualified Network.NFS.V4 as NFS
import qualified Network.Wai as Wai
import           Waimwork.HTTP (splitHTTP, unquoteHTTP, quoteHTTP, parseHTTPDate, formatHTTPDate)

import           Network.WebDAV.NFS.Types
import           Network.WebDAV.NFS.File

streamFile :: NFSRoot -> NFS.FileReference -> Word64 -> Word64 -> Wai.StreamingBody
streamFile nfs ref start end send done = do
  NFS.READ4res'NFS4_OK (NFS.READ4resok eof lbuf) <- NFS.nfsCall (nfsClient nfs)
    $ NFS.opFileReference ref *> NFS.op (NFS.READ4args NFS.anonymousStateid start $ fromIntegral l)
  let buf = NFS.unLengthArray lbuf
  send $ BSB.byteString buf
  let next = start + fromIntegral (BS.length buf)
  if next >= end || eof
    then done
    else streamFile nfs ref next end send done
  where
  r = end - start
  l = r `min` fromIntegral (nfsBlockSize nfs)

httpGET :: NFSRoot -> Wai.Request -> NFS.FileReference -> IO Wai.Response
httpGET nfs req pathref = do
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
    Just _ -> errorResponse HTTP.notImplemented501 -- "multipart/byteranges"
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
