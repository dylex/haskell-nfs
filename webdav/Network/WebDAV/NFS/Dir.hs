module Network.WebDAV.NFS.Dir
  ( readDir_
  ) where

import           Control.Exception (try)
import           Control.Monad (mfilter)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Bits ((.|.))
import qualified Network.NFS.V4 as NFS
import           Network.ONCRPC.XDR.Array (constLengthArray)
import           Network.ONCRPC.XDR.Opaque (fromOpaque)

import           Network.WebDAV.NFS.Types
import           Network.WebDAV.NFS.Response
import           Network.WebDAV.NFS.File

readDir_ :: MonadIO m => (Context -> m ()) -> Context -> m (Maybe DAVError)
readDir_ ent ctx = loop (constLengthArray 0) 0 where
  loop verf cook = either (return . Just) got =<< liftIO (try
    $ nfsFileCall ctx
      $ NFS.rEADDIR4res'resok4 <$> NFS.op (NFS.READDIR4args cook verf (count `div` 4) count
        (NFS.encodeBitmap $ fileInfoBitmap .|. NFS.packBitmap [NFS.AttrTypeRdattrError])))
  got (NFS.READDIR4resok _ (NFS.Dirlist4 Nothing _)) = return Nothing
  got (NFS.READDIR4resok verf (NFS.Dirlist4 (Just dl) eof)) = do
    cook <- list dl
    if eof
      then return Nothing
      else loop verf cook
  list (NFS.Entry4 cook name attrs next) = do
    mapM_ ent $ do
      name' <- maybe (Left "error") Right $ mfilter (validFileName (context ctx)) $ fromOpaque name
      attrs' <- decodeFileInfo attrs
      return ctx{ contextFile = attrs'{ filePath = filePath (contextFile ctx) ++ [name'] } }
    maybe (return cook) list next
  count = nfsBlockSize $ context ctx
