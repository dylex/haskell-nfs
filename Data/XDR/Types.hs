-- | XDR: External Data Representation Types

module Data.XDR.Types
  ( Int
  , UnsignedInt
  , Hyper
  , UnsignedHyper
  , Float
  , Double
  , Bool
  , Optional
  )
  where

import Prelude (Float, Double, Bool, Maybe)

import           Data.Int (Int32, Int64)
import           Data.Word (Word32, Word64)

type Int = Int32
type UnsignedInt = Word32
type Hyper = Int64
type UnsignedHyper = Word64
type Optional a = Maybe a
