{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
-- {-# OPTIONS_GHC -ddump-splices #-}
module Network.NFS.V4.Attr
  ( Bitmap
  , encodeBitmap
  , decodeBitmap
  , packBitmap
  , unpackBitmap
  , enpackBitmap
  , depackBitmap
  , AttrType(..)
  , AttrVal(..)
  , encodeAttrs
  , decodeAttrs
  , decodeTime
  , encodeTime
  ) where

import           Control.Arrow ((&&&), first, second)
import           Data.Bits ((.|.), bit, complementBit, finiteBitSize, shiftL, shiftR, testBit, zeroBits)
import           Data.Fixed (Fixed(..), Nano)
import           Data.Function (on)
import qualified Data.List as List
import qualified Data.Serialize as S
import           Data.Time.Clock.POSIX (POSIXTime)
import qualified Data.Vector as V
import qualified Network.ONCRPC as RPC
import           Network.ONCRPC.XDR.Array (lengthArray', unLengthArray)
import           Numeric.Natural (Natural)

import qualified Network.NFS.V4.Prot as NFS
import           Network.NFS.V4.Attr.TH

type Bitmap = Natural

encodeBitmap :: Bitmap -> NFS.Bitmap4
encodeBitmap = lengthArray' . V.unfoldr eb where
  eb 0 = Nothing
  eb n = Just (x, n `shiftR` finiteBitSize x) where
    x = fromIntegral n

decodeBitmap :: NFS.Bitmap4 -> Bitmap
decodeBitmap = V.foldr (\x n -> (n `shiftL` finiteBitSize x) .|. fromIntegral x) zeroBits . unLengthArray

packBitmap :: RPC.XDREnum a => [a] -> Bitmap
packBitmap [] = zeroBits
packBitmap (x:l) = bit (fromIntegral $ RPC.xdrFromEnum x) .|. packBitmap l

-- |Find all the bits set in the given bitmap corresponding to the given enum, and return the values and leftover bits.
-- There should be a better way to do this bit-search-explode operation.
unpackBitmap :: RPC.XDREnum a => Bitmap -> ([a], Bitmap)
unpackBitmap 0 = ([], 0)
unpackBitmap b = ub b 0 where
  ub 0 _ = ([], 0)
  ub n i = (if testBit n i
    then maybe (second (bit i .|.)) (first . (:)) (RPC.xdrToEnum $ fromIntegral i) . ub (complementBit n i)
    else ub n) (succ i)

enpackBitmap :: RPC.XDREnum a => [a] -> NFS.Bitmap4
enpackBitmap = encodeBitmap . packBitmap

depackBitmap :: RPC.XDREnum a => NFS.Bitmap4 -> ([a], Bitmap)
depackBitmap = unpackBitmap . decodeBitmap

thAttr
  [ "SUPPORTED_ATTRS"
  , "TYPE"
  , "FH_EXPIRE_TYPE"
  , "CHANGE"
  , "SIZE"
  , "LINK_SUPPORT"
  , "SYMLINK_SUPPORT"
  , "NAMED_ATTR"
  , "FSID"
  , "UNIQUE_HANDLES"
  , "LEASE_TIME"
  , "RDATTR_ERROR"
  , "ACL"
  , "ACLSUPPORT"
  , "ARCHIVE"
  , "CANSETTIME"
  , "CASE_INSENSITIVE"
  , "CASE_PRESERVING"
  , "CHOWN_RESTRICTED"
  , "FILEHANDLE"
  , "FILEID"
  , "FILES_AVAIL"
  , "FILES_FREE"
  , "FILES_TOTAL"
  , "FS_LOCATIONS"
  , "HIDDEN"
  , "HOMOGENEOUS"
  , "MAXFILESIZE"
  , "MAXLINK"
  , "MAXNAME"
  , "MAXREAD"
  , "MAXWRITE"
  , "MIMETYPE"
  , "MODE"
  , "NO_TRUNC"
  , "NUMLINKS"
  , "OWNER"
  , "OWNER_GROUP"
  , "QUOTA_AVAIL_HARD"
  , "QUOTA_AVAIL_SOFT"
  , "QUOTA_USED"
  , "RAWDEV"
  , "SPACE_AVAIL"
  , "SPACE_FREE"
  , "SPACE_TOTAL"
  , "SPACE_USED"
  , "SYSTEM"
  , "TIME_ACCESS"
  , "TIME_ACCESS_SET"
  , "TIME_BACKUP"
  , "TIME_CREATE"
  , "TIME_DELTA"
  , "TIME_METADATA"
  , "TIME_MODIFY"
  , "TIME_MODIFY_SET"
  , "MOUNTED_ON_FILEID"
  , "DIR_NOTIF_DELAY"
  , "DIRENT_NOTIF_DELAY"
  , "DACL"
  , "SACL"
  , "CHANGE_POLICY"
  , "FS_STATUS"
  , "FS_LAYOUT_TYPES"
  , "LAYOUT_HINT"
  , "LAYOUT_TYPES"
  , "LAYOUT_BLKSIZE"
  , "LAYOUT_ALIGNMENT"
  , "FS_LOCATIONS_INFO"
  , "MDSTHRESHOLD"
  , "RETENTION_GET"
  , "RETENTION_SET"
  , "RETENTEVT_GET"
  , "RETENTEVT_SET"
  , "RETENTION_HOLD"
  , "MODE_SET_MASKED"
  , "SUPPATTR_EXCLCREAT"
  , "FS_CHARSET_CAP"
  ]

instance RPC.XDR AttrType where
  xdrType _ = "AttrType"
  xdrPut = RPC.xdrPutEnum
  xdrGet = RPC.xdrGetEnum

decodeAttrs :: NFS.Fattr4 -> Either String [AttrVal]
decodeAttrs (NFS.Fattr4 m o) = S.runGet (mapM getAttr l) $ RPC.unOpaqueString $ unLengthArray o
  where (l, _) = depackBitmap m

encodeAttrs :: [AttrVal] -> NFS.Fattr4
encodeAttrs al = NFS.Fattr4 (enpackBitmap $ map fst l) (lengthArray' $ RPC.OpaqueString $ S.runPut $ mapM_ snd l)
  where l = map head $ List.groupBy ((==) `on` fst) $ List.sortOn fst $ map (attrType &&& putAttr) al

decodeTime :: NFS.Nfstime4 -> POSIXTime
decodeTime (NFS.Nfstime4 s n) = realToFrac $ (fromIntegral s :: Nano) + MkFixed (toInteger n)

encodeTime :: POSIXTime -> NFS.Nfstime4
encodeTime t = NFS.Nfstime4 (fromInteger s) (fromInteger n) where (s, MkFixed n) = properFraction (realToFrac t :: Nano)
