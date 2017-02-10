{-# LANGUAGE RecordWildCards #-}
module Network.WebDAV.NFS.PROPFIND
  ( httpPROPFIND
  ) where

import           Control.Exception (try)
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

propFindFile :: Context -> Maybe PropertySet -> FileInfo -> IO Response
propFindFile ctx pset fi = fmap res $ try $ do
  checkFileInfo 0 fi
  return $ maybe
    (return $ okPropStat $ mapProperty propertyContent <$> Set.toList standardProperties)
    (fileProps fi)
    pset
  where
  res x = ResponseProp (requestHRef ctx) (either
    (\e -> [PropStat (mapProperty propertyContent <$> foldMap Set.toList pset) (davErrorStatus e) (davErrorElements e) Nothing])
    id x) [] Nothing Nothing

propFindDirlist :: Context -> Maybe PropertySet -> Depth -> NFS.Entry4 -> C.ConduitM () Response IO NFS.Nfs_cookie4
propFindDirlist ctx pset depth (NFS.Entry4 cook name attrs next) = do
  mapM_ (\ctx' -> propFindEntry ctx' pset depth $ decodeFileInfo attrs)
    $ subContext ctx =<< fromOpaque name
  maybe (return cook) (propFindDirlist ctx pset depth) next

propFindDir :: Context -> Maybe PropertySet -> Depth -> NFS.FileHandle -> NFS.Verifier4 -> NFS.Nfs_cookie4 -> C.Source IO Response
propFindDir ctx pset depth dh verf cook = do
  NFS.READDIR4resok verf' (NFS.Dirlist4 dl eof) <- lift $ nfsCall ctx $ NFS.op (NFS.PUTFH4args dh)
    *> (NFS.rEADDIR4res'resok4 <$> NFS.op (NFS.READDIR4args cook verf (count `div` 4) count
      (NFS.encodeBitmap $ fileInfoBitmap .|. NFS.packBitmap [NFS.AttrTypeRdattrError])))
  cook' <- mapM (propFindDirlist ctx pset depth) dl
  mapM_ (propFindDir ctx pset depth dh verf') $ guard (not eof) >> cook'
  where
  count = nfsBlockSize $ contextNFS ctx

propFindEntry :: Context -> Maybe PropertySet -> Depth -> FileInfo -> C.Source IO Response
propFindEntry ctx pset depth fi = do
  C.yieldM $ propFindFile ctx pset fi
  when (depth > Depth0 && fileType fi == Just NFS.NF4DIR)
    $ propFindDir ctx pset (predDepth depth) (fileHandle fi) (constLengthArray 0) 0

httpPROPFIND :: Context -> IO Wai.Response
httpPROPFIND ctx = do
  when (depth == DepthInfinity) $ result $ errorResponse PropfindFiniteDepth
  req <- propFindSet . fromMaybe (PropFind True []) <$> requestXML ctx
  fi <- nfsCall ctx $ getFileInfo $ contextPath ctx
  checkFileInfo NFS.aCCESS4_READ fi
  return $ xmlStreamResponse multiStatus207 [] $ streamMultiStatus (propFindEntry ctx req depth fi) Nothing
  where
  depth = fromMaybe Depth1 {- FIXME: Infinity -} $ requestDepth ctx
