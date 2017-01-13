-- | XDR: External Data Representation Types

module Data.XDR.Types
  where

import Prelude ()

import           Data.Int (Int32, Int64)
import           Data.Word (Word32, Word64)

type Int = Int32
type UnsignedInt = Word32
type Hyper = Int64
type UnsignedHyper = Word64
