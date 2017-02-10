{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Network.WebDAV.NFS.GET
  ( httpGET
  ) where

import           Control.Applicative ((<|>))
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
import           Waimwork.HTTP (parseHTTPDate, formatHTTPDate, parseETag, parseETags, renderETag, matchETag)

import           Network.WebDAV.NFS.Types
import           Network.WebDAV.NFS.Request
import           Network.WebDAV.NFS.Response
import           Network.WebDAV.NFS.File

streamFile :: Context -> NFS.FileHandle -> Word64 -> Word64 -> Wai.StreamingBody
streamFile ctx fh start end send done = do
  NFS.READ4res'NFS4_OK (NFS.READ4resok eof lbuf) <- NFS.nfsCall (nfsClient $ contextNFS ctx)
    $ NFS.op (NFS.PUTFH4args fh) *> NFS.op (NFS.READ4args NFS.anonymousStateid start $ fromIntegral l)
  let buf = NFS.unLengthArray lbuf
  send $ BSB.byteString buf
  let next = start + fromIntegral (BS.length buf)
  if next >= end || eof
    then done
    else streamFile ctx fh next end send done
  where
  r = end - start
  l = r `min` fromIntegral (nfsBlockSize $ contextNFS ctx)

httpGET :: Context -> IO Wai.Response
httpGET ctx = do
  fi@FileInfo{..} <-
    nfsCall ctx
      $ getFileInfo $ contextPath ctx
  checkFileInfo NFS.aCCESS4_READ fi
  when (fileType /= Just NFS.NF4REG) $
    result $ statusResponse HTTP.methodNotAllowed405
  let headers =
        [ (HTTP.hETag, renderETag fileETag)
        , (HTTP.hLastModified, formatHTTPDate fileMTime)
        , (HTTP.hAcceptRanges, "bytes")
        ]
      ismat   = all (matchETag fileETag) ifmat
      isnomat = all (not . matchETag fileETag) ifnomat
      ismod   = all (fileMTime >) ifmod
      isnomod = all (fileMTime <=) ifnomod
      isrange = all (either (fileETag ==) (fileMTime <=)) ifrange
      ranges' = guard isrange >> mapMaybe (checkr . clampr (toInteger fileSize)) <$> ranges
      sizeb = BSB.word64Dec fileSize
  unless (isnomat || ismod) $
    result $ emptyResponse HTTP.notModified304 headers
  unless (ismat || isnomod) $
    result $ emptyResponse HTTP.preconditionFailed412 headers
  return $ case ranges' of
    Nothing -> Wai.responseStream HTTP.ok200
      ((HTTP.hContentLength, buildBS sizeb) : headers)
      (streamFile ctx fileHandle 0 fileSize)
    Just [] -> emptyResponse HTTP.requestedRangeNotSatisfiable416
      $ (HTTP.hContentRange, buildBS $ "bytes */" <> sizeb) : headers
    Just [(a,b)] -> Wai.responseStream HTTP.partialContent206
      ( (HTTP.hContentLength, buildBS $ BSB.word64Dec (succ b - a))
      : (HTTP.hContentRange, buildBS $ "bytes " <> BSB.word64Dec a <> BSB.char8 '-' <> BSB.word64Dec b <> BSB.char8 '/' <> sizeb)
      : headers)
      (streamFile ctx fileHandle a $ succ b)
    Just _ -> statusResponse HTTP.notImplemented501 -- "multipart/byteranges"
  where
  ifmat   = parseETags    <$> header HTTP.hIfMatch
  ifnomat = parseETags    <$> header HTTP.hIfNoneMatch
  ifmod   = parseHTTPDate =<< header HTTP.hIfModifiedSince
  ifnomod = parseHTTPDate =<< header HTTP.hIfUnmodifiedSince
  ifrange = (\s -> Right <$> parseHTTPDate s <|> Left <$> either (const Nothing) Just (parseETag s)) =<< header HTTP.hIfRange
  ranges  = HTTP.parseByteRanges =<< header HTTP.hRange
  header = requestHeader ctx
  clampr z (HTTP.ByteRangeFrom a) = (a `max` 0, pred z)
  clampr z (HTTP.ByteRangeFromTo a b) = (a `max` 0, b `min` pred z)
  clampr z (HTTP.ByteRangeSuffix e) = (z - e `max` 0, pred z)
  checkr (a, b)
    | a <= b = Just (fromInteger a, fromInteger b)
    | otherwise = Nothing
