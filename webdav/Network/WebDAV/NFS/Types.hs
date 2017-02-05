{-# LANGUAGE OverloadedStrings #-}
module Network.WebDAV.NFS.Types
  ( NFSRoot(..)
  , Context(..)
  , buildBS
  ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy as BSL
import           Data.Word (Word32)
import qualified Network.NFS.V4 as NFS
import qualified Network.Wai as Wai

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
  }

buildBS :: BSB.Builder -> BS.ByteString
buildBS = BSL.toStrict . BSB.toLazyByteString
