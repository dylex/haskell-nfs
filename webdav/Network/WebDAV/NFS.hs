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
import           Network.WebDAV.NFS.Response
import           Network.WebDAV.NFS.File
import           Network.WebDAV.NFS.GET

webDAVNFS :: NFSRoot -> Wai.Application
webDAVNFS nfs = resultApplication $ \req -> do
  let ctx = Context nfs req
  pathref <- maybe
    (result $ statusResponse HTTP.notFound404)
    (return . NFS.relativeFileReference (nfsRoot nfs))
    $ parsePath nfs $ Wai.pathInfo req
  case Wai.requestMethod req of
    "GET" -> httpGET ctx pathref
    "HEAD" -> do
      r <- httpGET ctx pathref
      return $ emptyResponse (Wai.responseStatus r) (Wai.responseHeaders r)
    _ -> return $ statusResponse HTTP.methodNotAllowed405
