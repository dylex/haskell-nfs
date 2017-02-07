{-# LANGUAGE OverloadedStrings #-}
module Network.WebDAV.NFS.Request
  ( requestHeader
  , requestDepth
  , requestHRef
  , requestXML
  ) where

import           Control.Exception (catches, Handler(..))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy.Char8 as BSLC
import qualified Data.Conduit as C
import           Data.Conduit.Attoparsec (ParseError)
import qualified Network.HTTP.Types as HTTP
import           Network.URI (nullURI, uriPath)
import qualified Network.Wai as Wai
import qualified Network.Wai.Conduit as WaiC
import           Network.Wai.Parse (parseContentType)
import           Text.Read (readMaybe)
import qualified Text.XML.Stream.Parse as XP
import           Waimwork.HTTP (unquoteHTTP, encodePathSegments')

import           Network.WebDAV.XML
import           Network.WebDAV.DAV
import           Network.WebDAV.NFS.Types
import           Network.WebDAV.NFS.Response

requestHeader :: Context -> HTTP.HeaderName -> Maybe BS.ByteString
requestHeader c h = lookup h $ Wai.requestHeaders $ contextRequest c

requestDepth :: Context -> Maybe Depth
requestDepth c = foldMap (readMaybe . BSC.unpack . unquoteHTTP) $ requestHeader c "depth"

requestHRef :: Context -> HRef
requestHRef ctx = nullURI
  { uriPath = BSLC.unpack $ BSB.toLazyByteString $ encodePathSegments' $ Wai.pathInfo $ contextRequest ctx
  }

requestXML :: XML a => Context -> IO (Maybe a)
requestXML ctx = mapM (dct . fst . parseContentType) $ requestHeader ctx HTTP.hContentType where
  dct "application/xml" = drc
  dct "text/xml" = drc
  dct _ = result $ statusResponse HTTP.unsupportedMediaType415
  drc = catches
    (WaiC.sourceRequestBody (contextRequest ctx) C.$$ xmlParser)
    [ Handler (result . exceptionResponse HTTP.badRequest400 :: ParseError -> IO a)
    , Handler (result . exceptionResponse unprocessableEntity422 :: XP.XmlException -> IO a)
    ]
