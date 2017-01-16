-- | XDR: External Data Representation Types

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
  , Array(..)
  , FixedOpaque
  , Opaque(..)
  , FixedString
  , String(..)
  , Optional
  )
  where

import Prelude hiding (Int, String)

import           Data.ByteString (ByteString)
import           Data.Function (on)
import           Data.Int (Int32, Int64)
import           Data.Word (Word32, Word64)
import           Data.Sext (Sext, unwrap)
import           Data.String (IsString)
import           GHC.TypeLits (Nat)

type Int = Int32
type UnsignedInt = Word32
type Hyper = Int64
type UnsignedHyper = Word64
type FixedArray (n :: Nat) a = Sext n [a] -- Vector a
instance Eq a => Eq (FixedArray n a) where
  (==) = (==) `on` unwrap
  (/=) = (/=) `on` unwrap
-- |Max length is recorded in the type but not enforced: most uses truncate
newtype Array (n :: Nat) a = Array [a]
  deriving (Eq, Ord, Functor, Applicative, Monad, Foldable, Monoid, Show)
type FixedOpaque (n :: Nat) = Sext n ByteString
instance Eq (FixedOpaque n) where
  (==) = (==) `on` unwrap
  (/=) = (/=) `on` unwrap
-- |Max length is recorded in the type but not enforced: most uses truncate
newtype Opaque (n :: Nat) = Opaque ByteString
  deriving (Eq, Ord, Monoid, Show, IsString)
type FixedString (n :: Nat) = Sext n ByteString
-- |Max length is recorded in the type but not enforced: most uses truncate
newtype String (n :: Nat) = String ByteString
  deriving (Eq, Ord, Monoid, Show, IsString)
type Optional a = Maybe a
