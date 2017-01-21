{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
-- {-# OPTIONS_GHC -Wno-redundant-constraints #-}
module Data.XDR.Array
  ( LengthArray
  , FixedLengthArray
  , fixedLengthArrayLength
  , BoundedLengthArray
  , boundedLengthArrayBound
  , unLengthArray
  , unsafeLengthArray
  , lengthArray
  , lengthArray'
  , boundLengthArray
  , emptyFixedLengthArray
  , emptyBoundedLengthArray
  , expandBoundedLengthArray
  , boundFixedLengthArray
  ) where

import           Prelude hiding (length, take, drop)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.List as List
import           Data.Maybe (fromMaybe, fromJust)
import           Data.Monoid (Monoid)
import           Data.Proxy (Proxy(..))
import           Data.String (IsString(..))
import qualified Data.Vector as V
import           GHC.TypeLits (Nat, KnownNat, natVal, type (+), type CmpNat)

class Monoid a => Array a where
  length :: a -> Int
  -- |Equivalent to @'compare' . 'length'@ but allows more efficient implementations
  compareLength :: a -> Int -> Ordering
  compareLength = compare . length
  take :: Int -> a -> a

instance Array [a] where
  length = List.length
  compareLength [] n = compare 0 n
  compareLength (_:l) n = compareLength l (n - 1)
  take = List.take

instance Array (V.Vector a) where
  length = V.length
  take = V.take

instance Array BS.ByteString where
  length = BS.length
  take = BS.take

instance Array BSL.ByteString where
  length = fromIntegral . BSL.length
  compareLength b n
    | BSL.null b' = LT
    | BSL.null (BSL.tail b') = EQ
    | otherwise = GT
    where b' = BSL.drop (fromIntegral n - 1) b
  take = BSL.take . fromIntegral

class KnownOrdering (o :: Ordering) where
  orderingVal :: proxy o -> Ordering

instance KnownOrdering 'LT where orderingVal _ = LT
instance KnownOrdering 'EQ where orderingVal _ = EQ
instance KnownOrdering 'GT where orderingVal _ = GT

-- |Assertion that the contained array satisfies @'compareLength' a n = o@
newtype LengthArray (o :: Ordering) (n :: Nat) a = LengthArray{ unLengthArray :: a }
  deriving (Eq, Ord, Show)

type FixedLengthArray n a = LengthArray 'EQ n a
type BoundedLengthArray n a = LengthArray 'LT (n + 1) a

lengthArrayOrdering :: forall o n a . KnownOrdering o => LengthArray o n a -> Ordering
lengthArrayOrdering _ = orderingVal (Proxy :: Proxy o)

lengthArrayBound :: forall o n a . KnownNat n => LengthArray o n a -> Int
lengthArrayBound _ = fromInteger $ natVal (Proxy :: Proxy n)

orderingOp :: Ordering -> Char
orderingOp LT = '<'
orderingOp EQ = '='
orderingOp GT = '>'

describeLengthArray :: (KnownOrdering o, KnownNat n) => LengthArray o n a -> String
describeLengthArray a = orderingOp (lengthArrayOrdering a) : show (lengthArrayBound a)

fixedLengthArrayLength :: KnownNat n => LengthArray 'EQ n a -> Int
fixedLengthArrayLength = lengthArrayBound

boundedLengthArrayBound :: KnownNat n => LengthArray 'LT n a -> Int
boundedLengthArrayBound = subtract 1 . lengthArrayBound

checkLengthArray :: (KnownOrdering o, KnownNat n, Array a) => LengthArray o n a -> Bool
checkLengthArray l@(LengthArray a) = compareLength a (lengthArrayBound l) == lengthArrayOrdering l

unsafeLengthArray :: a -> LengthArray o n a
unsafeLengthArray = LengthArray

lengthArray :: forall o n a . (KnownOrdering o, KnownNat n, Array a) => a -> Maybe (LengthArray o n a)
lengthArray a
  | checkLengthArray l = Just l
  | otherwise = Nothing
  where l = LengthArray a :: LengthArray o n a

lengthArray' :: forall o n a . (KnownOrdering o, KnownNat n, Array a) => a -> LengthArray o n a
lengthArray' a = fromMaybe (error $ "lengthArray': fails check " ++ describeLengthArray (fromJust la)) la
  where la = lengthArray a

boundLengthArray :: (KnownNat n, Array a) => a -> LengthArray 'LT n a
boundLengthArray a = l where
  l = LengthArray $ take (boundedLengthArrayBound l) a

instance (IsString a, KnownOrdering o, KnownNat n, Array a) => IsString (LengthArray o n a) where
  fromString s = fromMaybe
    (error $ "String " ++ show s ++ " fails LengthArray check " ++ describeLengthArray (fromJust ls))
    ls
    where ls = lengthArray $ fromString s

emptyFixedLengthArray :: Array a => LengthArray 'EQ 0 a
emptyFixedLengthArray = LengthArray mempty

emptyBoundedLengthArray :: (CmpNat 0 n ~ 'LT, Array a) => LengthArray 'LT n a
emptyBoundedLengthArray = LengthArray mempty

expandBoundedLengthArray :: (CmpNat n m ~ 'LT, Array a) => LengthArray 'LT n a -> LengthArray 'LT m a
expandBoundedLengthArray = LengthArray . unLengthArray

boundFixedLengthArray :: (CmpNat n m ~ 'LT, Array a) => LengthArray 'EQ n a -> LengthArray 'LT m a
boundFixedLengthArray = LengthArray . unLengthArray
