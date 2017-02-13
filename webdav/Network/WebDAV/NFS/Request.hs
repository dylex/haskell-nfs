{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Network.WebDAV.NFS.Request
  ( requestHeader
  , requestDepth
  , requestXML
  ) where

import           Control.Exception (catches, Handler(..))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Conduit as C
import           Data.Conduit.Attoparsec (ParseError)
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai
import qualified Network.Wai.Conduit as WaiC
import           Network.Wai.Parse (parseContentType)
import           Text.Read (readMaybe)
import qualified Text.XML.Stream.Parse as XP
import           Waimwork.HTTP (unquoteHTTP)

import           Network.WebDAV.XML
import           Network.WebDAV.DAV
import           Network.WebDAV.NFS.Types
import           Network.WebDAV.NFS.Response

requestHeader :: Context -> HTTP.HeaderName -> Maybe BS.ByteString
requestHeader c h = lookup h $ Wai.requestHeaders $ contextRequest c

requestDepth :: Context -> Maybe Depth
requestDepth c = foldMap (readMaybe . BSC.unpack . unquoteHTTP) $ requestHeader c "depth"

requestXML :: XML a => Context -> IO (Maybe a)
requestXML ctx = mapM (dct . fst . parseContentType) $ requestHeader ctx HTTP.hContentType where
  dct "application/xml" = drc
  dct "text/xml" = drc
  dct _ = throwHTTP HTTP.unsupportedMediaType415
  drc = catches
    (WaiC.sourceRequestBody (contextRequest ctx) C.$$ xmlParser)
    [ Handler (\(_ :: ParseError) -> throwHTTP HTTP.badRequest400)
    , Handler (\(_ :: XP.XmlException) -> throwHTTP unprocessableEntity422)
    ]
