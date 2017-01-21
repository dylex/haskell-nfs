-- | XDR Types

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Data.XDR.Types
  ( Int
  , UnsignedInt
  , Hyper
  , UnsignedHyper
  , Float
  , Double
  , Bool
  , FixedArray
  , Array
  , FixedOpaque
  , Opaque
  , FixedString
  , String
  , Optional

  , Length
  , maxLength
  )
  where

import Prelude hiding (Int, String)

import           Data.ByteString (ByteString)
import           Data.Int (Int32, Int64)
import           Data.Vector (Vector)
import           Data.Word (Word32, Word64)

import           Data.XDR.Array

type Int            = Int32
type UnsignedInt    = Word32
type Hyper          = Int64
type UnsignedHyper  = Word64
type FixedArray n a =   FixedLengthArray n (Vector a)
type      Array n a = BoundedLengthArray n (Vector a)
type FixedOpaque n  =   FixedLengthArray n ByteString
type      Opaque n  = BoundedLengthArray n ByteString
type FixedString n  =   FixedLengthArray n ByteString
type      String n  = BoundedLengthArray n ByteString
type Optional a     = Maybe a

-- |Not a real XDR type, but used for length headers
type Length = UnsignedInt

maxLength :: Length
maxLength = maxBound
