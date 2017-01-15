-- | XDR: External Data Representation Types

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
module Data.XDR.Types
  ( Int
  , UnsignedInt
  , Hyper
  , UnsignedHyper
  , Float
  , Double
  , Bool
  , FixedArray
  , Array(..)
  , FixedOpaque
  , Opaque(..)
  , FixedString
  , String(..)
  , Optional
  )
  where

import Prelude (Float, Double, Bool, Maybe, Eq, Ord, Functor, Applicative, Monad, Foldable, Monoid, Show)

import           Data.ByteString (ByteString)
import           Data.Int (Int32, Int64)
import           Data.Word (Word32, Word64)
import           Data.Sext (Sext)
import           Data.String (IsString)
import           GHC.TypeLits (Nat)

type Int = Int32
type UnsignedInt = Word32
type Hyper = Int64
type UnsignedHyper = Word64
type FixedArray (n :: Nat) a = Sext n [a] -- Vector a
-- |Max length is recorded in the type but not enforced: most uses truncate
newtype Array (n :: Nat) a = Array [a]
  deriving (Eq, Ord, Functor, Applicative, Monad, Foldable, Monoid, Show)
type FixedOpaque (n :: Nat) = Sext n ByteString
-- |Max length is recorded in the type but not enforced: most uses truncate
newtype Opaque (n :: Nat) = Opaque ByteString
  deriving (Eq, Ord, Monoid, Show, IsString)
type FixedString (n :: Nat) = Sext n ByteString
-- |Max length is recorded in the type but not enforced: most uses truncate
newtype String (n :: Nat) = String ByteString
  deriving (Eq, Ord, Monoid, Show, IsString)
type Optional a = Maybe a
