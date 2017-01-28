{-# LANGUAGE OverloadedStrings #-}
module Network.WebDAV.NFS
  ( webDAVNFS
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

import           Network.WebDAV.NFS.Types
import           Network.WebDAV.NFS.File
import           Network.WebDAV.NFS.GET

webDAVNFS :: NFSRoot -> Wai.Application
webDAVNFS nfs = resultApplication $ \req -> do
  pathref <- maybe (errorResult HTTP.notFound404) (return . NFS.relativeFileReference (nfsRoot nfs))
    $ parsePath nfs $ Wai.pathInfo req
  case Wai.requestMethod req of
    "GET" -> httpGET nfs req pathref
