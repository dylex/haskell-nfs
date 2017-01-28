{-# LANGUAGE OverloadedStrings #-}
module Network.WebDAV.NFS
  ( NFSRoot(..)
  , webDAVNFS
  ) where

import qualified Network.HTTP.Types as HTTP
import qualified Network.NFS.V4 as NFS
import qualified Network.Wai as Wai
import           Waimwork.Result (resultApplication)

import           Network.WebDAV.NFS.Types
import           Network.WebDAV.NFS.File
import           Network.WebDAV.NFS.GET

webDAVNFS :: NFSRoot -> Wai.Application
webDAVNFS nfs = resultApplication $ \req -> do
  pathref <- maybe (errorResult HTTP.notFound404) (return . NFS.relativeFileReference (nfsRoot nfs))
    $ parsePath nfs $ Wai.pathInfo req
  case Wai.requestMethod req of
    "GET" -> httpGET nfs req pathref
