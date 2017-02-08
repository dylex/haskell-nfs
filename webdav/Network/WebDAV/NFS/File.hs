{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module Network.WebDAV.NFS.File
  ( parsePath
  , checkAccess
  , FileInfo(..)
  , getFileInfo
  ) where

import           Control.Monad (unless)
import           Data.Bits ((.&.), (.|.))
import qualified Data.ByteString.Builder as BSB
import           Data.List (foldl')
import qualified Data.Text as T
import           Data.Time.Clock (UTCTime)
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import           Data.Word (Word64)
import qualified Network.HTTP.Types as HTTP
import           Network.ONCRPC.XDR.Array (emptyBoundedLengthArray)
import qualified Network.NFS.V4 as NFS
import           Waimwork.HTTP (ETag(..))

import           Network.WebDAV.NFS.Types
import           Network.WebDAV.NFS.Response

parsePath :: NFSRoot -> [T.Text] -> Maybe [NFS.FileName]
parsePath _ ("":_) = Nothing
parsePath _ (".":_) = Nothing
parsePath _ ("..":_) = Nothing
parsePath NFSRoot{ nfsDotFiles = False } ((T.head -> '.'):_) = Nothing
parsePath r (f:l) = (NFS.StrCS f :) <$> parsePath r l
parsePath _ [] = Just []

data FileInfo = FileInfo
  { fileHandle :: NFS.FileHandle
  , fileAccess :: NFS.Uint32_t
  , fileType :: NFS.Nfs_ftype4
  , fileETag :: ETag
  , fileSize :: Word64
  , fileCTime :: Maybe UTCTime
  , fileMTime :: UTCTime
  }

emptyFileInfo :: FileInfo
emptyFileInfo = FileInfo
  { fileHandle = emptyBoundedLengthArray
  , fileAccess = 0
  , fileType = NFS.NF4REG
  , fileETag = WeakETag ""
  , fileSize = 0
  , fileCTime = Nothing
  , fileMTime = posixSecondsToUTCTime 0
  }

updateFileAttr :: FileInfo -> NFS.AttrVal -> FileInfo
updateFileAttr f (NFS.AttrValType       x) = f{ fileType = x }
updateFileAttr f (NFS.AttrValChange     x) = f{ fileETag = StrongETag $ buildBS $ BSB.word64Hex x }
updateFileAttr f (NFS.AttrValSize       x) = f{ fileSize = x }
updateFileAttr f (NFS.AttrValTimeCreate x) = f{ fileCTime = Just $ posixSecondsToUTCTime $ NFS.decodeTime x }
updateFileAttr f (NFS.AttrValTimeModify x) = f{ fileMTime = posixSecondsToUTCTime $ NFS.decodeTime x }
updateFileAttr f _ = f

getFileInfo :: NFS.FileReference -> NFS.Ops FileInfo
getFileInfo fr = fi <$> (NFS.opFileReference fr
  *> NFS.opGetFileHandle)
  <*> (NFS.aCCESS4resok'access . NFS.aCCESS4res'resok4 <$> NFS.op
    (NFS.ACCESS4args $ NFS.aCCESS4_READ .|. NFS.aCCESS4_LOOKUP .|. NFS.aCCESS4_MODIFY .|. NFS.aCCESS4_EXTEND .|. NFS.aCCESS4_DELETE))
  <*> (NFS.decodeAttrs . NFS.gETATTR4resok'obj_attributes . NFS.gETATTR4res'resok4 <$> NFS.op (NFS.GETATTR4args $ NFS.enpackBitmap
    [ NFS.AttrTypeType
    , NFS.AttrTypeChange
    , NFS.AttrTypeSize
    , NFS.AttrTypeTimeCreate
    , NFS.AttrTypeTimeModify
    ]))
  where
  fi fh access (Right al@
    ( NFS.AttrValType _
    : NFS.AttrValChange _
    : NFS.AttrValSize _
    : _)) =
    foldl' updateFileAttr emptyFileInfo
      { fileHandle = fh
      , fileAccess = access
      } al
  fi _ _ e = error $ "GETATTR: " ++ show e -- 500 error

checkAccess :: NFS.Uint32_t -> FileInfo -> IO ()
checkAccess a i = unless (a .&. fileAccess i == a) $ throwDAVError $ DAVStatus HTTP.forbidden403
