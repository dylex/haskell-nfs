{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Network.ONCRPC.Types
  ( XID
  , ProgNum
  , VersNum
  , ProcNum
  , FragmentHeader
  , mkFragmentHeader
  , unFragmentHeader
  , splitFragments
  ) where

import           Data.Bits (Bits, bit, clearBit, setBit, testBit)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import           Data.Word (Word32)
import           Foreign.Storable (Storable)
import           Network.Socket (htonl, ntohl)

type XID = Word32
type ProgNum = Word32
type VersNum = Word32
type ProcNum = Word32

newtype FragmentHeader = FragmentHeader Word32
  deriving (Storable)

maxFragmentSize :: (Bits i, Integral i) => i
maxFragmentSize = bit 31

mkFragmentHeader :: (Bits i, Integral i) => Bool -> i -> FragmentHeader
mkFragmentHeader l x
  | x < 0 || x >= maxFragmentSize = error "mkFragmentHeader"
  | l = r $ setBit w 31
  | otherwise = r w
  where
  w = fromIntegral x
  r = FragmentHeader . htonl

unFragmentHeader :: Integral i => FragmentHeader -> (Bool, i)
unFragmentHeader (FragmentHeader w) = (testBit w' 31, fromIntegral $ clearBit w' 31) where w' = ntohl w

splitFragments :: BSL.ByteString -> [(FragmentHeader, BS.ByteString)]
splitFragments b = f : if l then [] else splitFragments t where
  (h, t) = BSL.splitAt maxFragmentSize b
  h' = BSL.toStrict h
  l = BSL.null t
  f = (mkFragmentHeader l $ BS.length h', h')
