{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module Network.WebDAV.NFS.Types
  ( NFSRoot(..)
  , Context(..)
  , validFileName
  , subContext
  , buildBS
  , nfsCall
  ) where

import           Control.Monad (guard)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import           Data.Word (Word32)
import qualified Network.NFS.V4 as NFS
import qualified Network.Wai as Wai

import           Network.WebDAV.NFS.Response

data NFSRoot = NFSRoot
  { nfsClient :: NFS.Client
  , nfsRoot :: NFS.FileReference
  , nfsReadOnly :: !Bool
  , nfsDotFiles :: !Bool
  , nfsBlockSize :: !Word32
  }

data Context = Context
  { contextNFS :: !NFSRoot
  , contextRequest :: !Wai.Request
  , contextPath :: NFS.FileReference
  }

validFileName :: NFSRoot -> T.Text -> Bool
validFileName _ "" = False
validFileName _ "." = False
validFileName _ ".." = False
validFileName NFSRoot{ nfsDotFiles = False } (T.head -> '.') = False
validFileName _ _ = True

subContext :: Context -> NFS.FileName -> Maybe Context
subContext ctx name = ctx
  { contextPath = NFS.FileLookup (contextPath ctx) name
  , contextRequest = (contextRequest ctx){ Wai.pathInfo = Wai.pathInfo (contextRequest ctx) ++ [tname] }
  } <$ guard (validFileName (contextNFS ctx) tname)
  where
  tname = NFS.strCSText name

buildBS :: BSB.Builder -> BS.ByteString
buildBS = BSL.toStrict . BSB.toLazyByteString

nfsCall :: Context -> NFS.Ops a -> IO a
nfsCall ctx = handleNFSException . NFS.nfsCall (nfsClient $ contextNFS ctx)
