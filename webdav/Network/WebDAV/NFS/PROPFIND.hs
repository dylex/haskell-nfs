module Network.WebDAV.NFS.PROPFIND
  ( httpPROPFIND
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
import           Network.WebDAV.NFS.File

httpPROPFIND :: Context -> NFS.FileReference -> IO Wai.Response
httpPROPFIND ctx pathref = do
  req <- decodeXMLRequest ctx
  where
  depth = requestDepth ctx
