{-# LANGUAGE OverloadedStrings #-}
module Network.WebDAV.NFS.XML
  ( decodeXMLRequest
  ) where

import           Control.Exception (catches, Handler(..))
import qualified Data.Conduit as C
import           Data.Conduit.Attoparsec (ParseError)
import           Network.HTTP.Types.Header (hContentType)
import           Network.HTTP.Types.Status (badRequest400, unsupportedMediaType415, unprocessableEntity422)
import           Network.Wai.Conduit (sourceRequestBody)
import           Network.Wai.Parse (parseContentType)
import qualified Text.XML.Stream.Parse as XP

import           Network.WebDAV.NFS.Types
import           Network.WebDAV.XML

decodeXMLRequest :: XML a => Context -> IO (Maybe a)
decodeXMLRequest ctx = mapM (dct . fst . parseContentType) $ requestHeader ctx hContentType where
  dct "application/xml" = drc
  dct "text/xml" = drc
  dct _ = errorResult unsupportedMediaType415
  drc = catches
    (sourceRequestBody (contextRequest ctx) C.$$ xmlParser)
    [ Handler (exceptionResult badRequest400 :: ParseError -> IO a)
    , Handler (exceptionResult unprocessableEntity422 :: XP.XmlException -> IO a)
    ]
