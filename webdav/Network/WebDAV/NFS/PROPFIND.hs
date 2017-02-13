{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Network.WebDAV.NFS.PROPFIND
  ( httpPROPFIND
  ) where

import           Control.Exception (try)
import           Control.Monad (when, void)
import qualified Data.Conduit as C
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
import           Network.WebDAV.NFS.Dir
import           Network.WebDAV.NFS.Property

okPropStat :: Prop Maybe -> PropStat
okPropStat l = PropStat l HTTP.ok200 [] Nothing

fileProps :: FileInfo -> PropertySet -> [PropStat]
fileProps fi = ps . partitionEithers . map gp . Set.toList where
  ps (n, v) =
    [ okPropStat v
    , PropStat (mapProperty propertyContent <$> n) HTTP.notFound404 [] Nothing
    ]
  gp pn
    | Just p <- fileInfoProperty pn fi = Right p
    | otherwise = Left pn

propFindFile :: Maybe PropertySet -> Context -> IO Response
propFindFile pset ctx = fmap res $ try $ do
  checkFileInfo 0 $ contextFile ctx
  return $ maybe
    (return $ okPropStat $ mapProperty propertyContent <$> Set.toList standardProperties)
    (fileProps $ contextFile ctx)
    pset
  where
  res x = ResponseProp (fileHRef ctx) (either
    (\e -> [PropStat (mapProperty propertyContent <$> foldMap Set.toList pset) (davErrorStatus e) (davErrorElements e) Nothing])
    id x) [] Nothing Nothing

propFindEntry :: Maybe PropertySet -> Depth -> Context -> C.Source IO Response
propFindEntry pset depth ctx = do
  C.yieldM $ propFindFile pset ctx
  when (depth > Depth0 && fileType (contextFile ctx) == Just NFS.NF4DIR)
    $ void $ readDir_ (propFindEntry pset (predDepth depth)) ctx

httpPROPFIND :: Context -> IO Wai.Response
httpPROPFIND ctx@Context{ contextFile = fi } = do
  -- when (depth == DepthInfinity) $ result $ errorResponse PropfindFiniteDepth
  req <- propFindSet . fromMaybe (PropFind True []) <$> requestXML ctx
  checkFileInfo NFS.aCCESS4_READ fi
  return $ xmlStreamResponse multiStatus207 [] $ streamMultiStatus (propFindEntry req depth ctx) Nothing
  where
  depth = fromMaybe DepthInfinity $ requestDepth ctx
