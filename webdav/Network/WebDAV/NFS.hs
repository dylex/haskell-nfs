{-# LANGUAGE OverloadedStrings #-}
module Network.WebDAV.NFS
  ( WebDAVNFS(..)
  , webDAVNFS
  , webDAVNFSApplication
  ) where

import           Data.List (stripPrefix)
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai

import           Network.WebDAV.NFS.Types
import           Network.WebDAV.NFS.Response
import           Network.WebDAV.NFS.File
import           Network.WebDAV.NFS.GET
import           Network.WebDAV.NFS.PROPFIND

webDAVNFS :: WebDAVNFS -> Wai.Request -> IO Wai.Response
webDAVNFS nfs req = handleDAV $ do
  path <- maybe 
    (throwHTTP HTTP.notFound404)
    return $ parsePath nfs =<< stripPrefix (webDAVRoot nfs) (Wai.pathInfo req)
  info <- getFileInfo nfs path
  let ctx = Context nfs req info
  Wai.mapResponseHeaders (("DAV", "1") :) <$>
    case Wai.requestMethod req of
      "OPTIONS" -> return $ emptyResponse HTTP.ok200 []
      "GET" -> httpGET ctx
      "HEAD" -> do
        r <- httpGET ctx
        return $ emptyResponse (Wai.responseStatus r) (Wai.responseHeaders r)
      "PROPFIND" -> httpPROPFIND ctx
      _ -> throwMethodNotAllowed ctx

webDAVNFSApplication :: WebDAVNFS -> Wai.Application
webDAVNFSApplication nfs = (>>=) . webDAVNFS nfs
