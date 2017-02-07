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
import           Network.WebDAV.NFS.PROPFIND

webDAVNFS :: NFSRoot -> Wai.Application
webDAVNFS nfs = resultApplication $ \req -> handleDAVError $ do
  ctx <- Context nfs req <$> maybe
    (result $ statusResponse HTTP.notFound404)
    (return . NFS.relativeFileReference (nfsRoot nfs))
    (parsePath nfs $ Wai.pathInfo req)
  Wai.mapResponseHeaders (("DAV", "1") :) <$>
    case Wai.requestMethod req of
      "OPTIONS" -> return $ statusResponse HTTP.ok200
      "GET" -> httpGET ctx
      "HEAD" -> do
        r <- httpGET ctx
        return $ emptyResponse (Wai.responseStatus r) (Wai.responseHeaders r)
      "PROPFIND" -> httpPROPFIND ctx
      _ -> return $ statusResponse HTTP.methodNotAllowed405
