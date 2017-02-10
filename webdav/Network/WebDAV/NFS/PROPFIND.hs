{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Network.WebDAV.NFS.PROPFIND
  ( httpPROPFIND
  ) where

import           Control.Exception (try, handle)
import           Control.Monad (guard, when)
import           Control.Monad.Trans.Class (lift)
import           Data.Bits ((.|.))
import qualified Data.Conduit as C
import           Data.Either (partitionEithers)
import           Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import qualified Network.HTTP.Types as HTTP
import qualified Network.NFS.V4 as NFS
import           Network.ONCRPC.XDR.Array (constLengthArray)
import           Network.ONCRPC.XDR.Opaque (fromOpaque)
import qualified Network.Wai as Wai

import           Network.WebDAV.DAV
import           Network.WebDAV.NFS.Types
import           Network.WebDAV.NFS.Request
import           Network.WebDAV.NFS.Response
import           Network.WebDAV.NFS.File
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
  res x = ResponseProp (requestHRef ctx) (either
    (\e -> [PropStat (mapProperty propertyContent <$> foldMap Set.toList pset) (davErrorStatus e) (davErrorElements e) Nothing])
    id x) [] Nothing Nothing

propFindDirlist :: Maybe PropertySet -> Depth -> Context -> NFS.Entry4 -> C.ConduitM () Response IO NFS.Nfs_cookie4
propFindDirlist pset depth ctx (NFS.Entry4 cook name attrs next) = do
  mapM_ (propFindEntry pset depth)
    $ subContext ctx (decodeFileInfo attrs) =<< fromOpaque name
  maybe (return cook) (propFindDirlist pset depth ctx) next

propFindDir :: Maybe PropertySet -> Depth -> Context -> NFS.Verifier4 -> NFS.Nfs_cookie4 -> C.Source IO Response
propFindDir pset depth ctx verf cook = do
  NFS.READDIR4resok verf' (NFS.Dirlist4 dl eof) <- lift $ handle
    (\(_ :: DAVError) -> return $ NFS.READDIR4resok verf (NFS.Dirlist4 Nothing False))
    $ nfsFileCall ctx
      $ NFS.rEADDIR4res'resok4 <$> NFS.op (NFS.READDIR4args cook verf (count `div` 4) count
        (NFS.encodeBitmap $ fileInfoBitmap .|. NFS.packBitmap [NFS.AttrTypeRdattrError]))
  cook' <- mapM (propFindDirlist pset depth ctx) dl
  mapM_ (propFindDir pset depth ctx verf') $ guard (not eof) >> cook'
  where
  count = nfsBlockSize $ contextNFS ctx

propFindEntry :: Maybe PropertySet -> Depth -> Context -> C.Source IO Response
propFindEntry pset depth ctx = do
  C.yieldM $ propFindFile pset ctx
  when (depth > Depth0 && fileType (contextFile ctx) == Just NFS.NF4DIR)
    $ propFindDir pset (predDepth depth) ctx (constLengthArray 0) 0

httpPROPFIND :: Context -> IO Wai.Response
httpPROPFIND ctx@Context{ contextFile = fi } = do
  -- when (depth == DepthInfinity) $ result $ errorResponse PropfindFiniteDepth
  req <- propFindSet . fromMaybe (PropFind True []) <$> requestXML ctx
  checkFileInfo NFS.aCCESS4_READ fi
  return $ xmlStreamResponse multiStatus207 [] $ streamMultiStatus (propFindEntry req depth ctx) Nothing
  where
  depth = fromMaybe DepthInfinity $ requestDepth ctx
