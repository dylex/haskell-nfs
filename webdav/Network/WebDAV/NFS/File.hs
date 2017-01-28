{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module Network.WebDAV.NFS.File
  ( parsePath
  , checkAccess
  , FileInfo(..)
  , getFileInfo
  ) where

import           Control.Monad (unless)
import           Data.Bits ((.|.))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.Text as T
import           Data.Time.Clock (UTCTime)
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import           Data.Word (Word64)
import qualified Network.HTTP.Types as HTTP
import qualified Network.NFS.V4 as NFS

import           Network.WebDAV.NFS.Types

parsePath :: NFSRoot -> [T.Text] -> Maybe [NFS.FileName]
parsePath _ ("":_) = Nothing
parsePath _ (".":_) = Nothing
parsePath _ ("..":_) = Nothing
parsePath NFSRoot{ nfsDotFiles = False } ((T.head -> '.'):_) = Nothing
parsePath r (f:l) = (NFS.StrCS f :) <$> parsePath r l
parsePath _ [] = Just []

checkAccess :: NFS.Uint32_t -> NFS.Ops (IO ())
checkAccess a = (`unless` errorResult HTTP.forbidden403) . (a ==) . NFS.aCCESS4resok'access . NFS.aCCESS4res'resok4 <$> NFS.op (NFS.ACCESS4args a)

data FileInfo = FileInfo
  { fileType :: NFS.Nfs_ftype4
  , fileETag :: BS.ByteString
  , fileSize :: Word64
  , fileMTime :: UTCTime
  }

getFileInfo :: NFS.Ops FileInfo
getFileInfo = fi . NFS.decodeAttrs . NFS.gETATTR4resok'obj_attributes . NFS.gETATTR4res'resok4
  <$> NFS.op (NFS.GETATTR4args $ NFS.encodeBitmap $
    NFS.fATTR4_TYPE .|. NFS.fATTR4_CHANGE .|. NFS.fATTR4_SIZE .|. NFS.fATTR4_TIME_MODIFY)
  where
  fi (Right [NFS.AttrValType ftyp, NFS.AttrValChange tag, NFS.AttrValSize size, NFS.AttrValTimeModify mtime]) =
    FileInfo ftyp (buildBS $ BSB.word64Hex tag) size (posixSecondsToUTCTime $ NFS.decodeTime mtime)
  fi e = error $ "GETATTR: " ++ show e -- 500 error

