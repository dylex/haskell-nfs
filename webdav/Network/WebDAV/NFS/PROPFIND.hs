{-# LANGUAGE RecordWildCards #-}
module Network.WebDAV.NFS.PROPFIND
  ( httpPROPFIND
  ) where

import           Control.Exception (try)
import           Control.Monad (when)
import           Data.Either (partitionEithers)
import           Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import qualified Network.HTTP.Types as HTTP
import qualified Network.NFS.V4 as NFS
import qualified Network.Wai as Wai

import           Network.WebDAV.DAV
import           Network.WebDAV.NFS.Types
import           Network.WebDAV.NFS.Request
import           Network.WebDAV.NFS.Response
import           Network.WebDAV.NFS.File
import           Network.WebDAV.NFS.Property

okPropStat :: Prop Maybe -> PropStat
okPropStat l = PropStat l HTTP.ok200 [] Nothing

responseProp :: Context -> Maybe PropertySet -> Either DAVError [PropStat] -> Response
responseProp ctx pset ep = ResponseProp (requestHRef ctx) (either
    (\e -> [PropStat (mapProperty propertyContent <$> foldMap Set.toList pset) (davErrorStatus e) (davErrorElements e) Nothing])
    id
  ep) [] Nothing Nothing

fileProps :: FileInfo -> PropertySet -> [PropStat]
fileProps fi = ps . partitionEithers . map gp . Set.toList where
  ps (n, v) =
    [ okPropStat v
    , PropStat (mapProperty propertyContent <$> n) HTTP.notFound404 [] Nothing
    ]
  gp pn
    | Just p <- fileInfoProperty pn fi = Right p
    | otherwise = Left pn

doPropFind :: Context -> Maybe PropertySet -> Depth -> IO Response
doPropFind ctx pset _depth = fmap (responseProp ctx pset) $ try $ do
  fi@FileInfo{..} <-
    nfsCall ctx
      $ getFileInfo $ contextPath ctx
  checkAccess NFS.aCCESS4_READ fi
  return $ maybe
    (return $ okPropStat $ mapProperty propertyContent <$> Set.toList standardProperties)
    (fileProps fi)
    pset

httpPROPFIND :: Context -> IO Wai.Response
httpPROPFIND ctx = do
  when (depth == DepthInfinity) $ result $ errorResponse PropfindFiniteDepth
  req <- propFindSet . fromMaybe (PropFind True []) <$> requestXML ctx
  xmlResponse HTTP.ok200 []
    <$> doPropFind ctx req depth
  where
  depth = fromMaybe Depth1 {- XXX: Infinity -} $ requestDepth ctx
