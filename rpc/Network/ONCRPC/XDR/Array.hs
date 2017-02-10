-- |Various kinds of arrays (lists, vectors, bytestrings) with statically aserted length constraints encoded in their type.

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
-- {-# OPTIONS_GHC -Wno-redundant-constraints #-}
module Network.ONCRPC.XDR.Array
  ( KnownNat
  , KnownOrdering
  , LengthArray
  , FixedLengthArray
  , fixedLengthArrayLength
  , BoundedLengthArray
  , boundedLengthArrayBound
  , unLengthArray
  , unsafeLengthArray
  , lengthArray
  , lengthArray'
  , boundLengthArray
  , boundLengthArrayFromList
  , padLengthArray
  , constLengthArray
  , emptyFixedLengthArray
  , emptyBoundedLengthArray
  , expandBoundedLengthArray
  , boundFixedLengthArray
  , appendLengthArray
  , fromLengthList
  ) where

import           Prelude hiding (length, take, drop, replicate)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.List as List
import           Data.Maybe (fromMaybe, fromJust)
import           Data.Monoid (Monoid, (<>))
import           Data.Proxy (Proxy(..))
import           Data.String (IsString(..))
import qualified Data.Vector as V
import           Data.Word (Word8)
import           GHC.TypeLits (Nat, KnownNat, natVal, type (+), type CmpNat)

class HasLength a where
  length :: a -> Int
  -- |Equivalent to @'compare' . 'length'@ but allows more efficient implementations
  compareLength :: a -> Int -> Ordering
  compareLength = compare . length

class (Monoid a, HasLength a) => Array a where
  type Elem a :: *
  take :: Int -> a -> a
  replicate :: Int -> Elem a -> a
  fromList :: [Elem a] -> a

instance HasLength [a] where
  length = List.length
  compareLength [] n = compare 0 n
  compareLength (_:l) n = compareLength l (n - 1)
instance Array [a] where
  type Elem [a] = a
  take = List.take
  replicate = List.replicate
  fromList = id

instance HasLength (V.Vector a) where
  length = V.length
instance Array (V.Vector a) where
  type Elem (V.Vector a) = a
  take = V.take
  replicate = V.replicate
  fromList = V.fromList

instance HasLength BS.ByteString where
  length = BS.length
instance Array BS.ByteString where
  type Elem BS.ByteString = Word8
  take = BS.take
  replicate = BS.replicate
  fromList = BS.pack

instance HasLength BSL.ByteString where
  length = fromIntegral . BSL.length
  compareLength b n
    | BSL.null b' = LT
    | BSL.null (BSL.tail b') = EQ
    | otherwise = GT
    where b' = BSL.drop (fromIntegral n - 1) b
instance Array BSL.ByteString where
  type Elem BSL.ByteString = Word8
  take = BSL.take . fromIntegral
  replicate = BSL.replicate . fromIntegral
  fromList = BSL.pack

class KnownOrdering (o :: Ordering) where
  orderingVal :: proxy o -> Ordering

instance KnownOrdering 'LT where orderingVal _ = LT
instance KnownOrdering 'EQ where orderingVal _ = EQ
instance KnownOrdering 'GT where orderingVal _ = GT

-- |Assertion that the contained array satisfies @'compareLength' a n = o@
newtype LengthArray (o :: Ordering) (n :: Nat) a = LengthArray{ unLengthArray :: a }
  deriving (Eq, Ord, Show)

instance HasLength a => HasLength (LengthArray o n a) where
  length = length . unLengthArray
  compareLength = compareLength . unLengthArray

-- |Assertion that the contained array is exactly a static length
type FixedLengthArray n a = LengthArray 'EQ n a
-- |Assertion that the contained array is at most a static length (inclusive)
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

-- |Static length of a 'FixedLengthArray'
fixedLengthArrayLength :: KnownNat n => LengthArray 'EQ n a -> Int
fixedLengthArrayLength = lengthArrayBound

-- |Static upper-bound (inclusive) of a 'BoundedLengthArray'
boundedLengthArrayBound :: KnownNat n => LengthArray 'LT n a -> Int
boundedLengthArrayBound = subtract 1 . lengthArrayBound

-- |Unsafely create a 'LengthArray' without checking the length bound assertion.
-- May cause unpredictable behavior if the bound does not hold.
unsafeLengthArray :: a -> LengthArray o n a
unsafeLengthArray = LengthArray

checkLengthArray :: (KnownOrdering o, KnownNat n, HasLength a) => LengthArray o n a -> Bool
checkLengthArray l@(LengthArray a) = compareLength a (lengthArrayBound l) == lengthArrayOrdering l

-- |Safely create a 'LengthArray' out of an array if it conforms to the static length assertion.
lengthArray :: forall o n a . (KnownOrdering o, KnownNat n, HasLength a) => a -> Maybe (LengthArray o n a)
lengthArray a
  | checkLengthArray l = Just l
  | otherwise = Nothing
  where l = LengthArray a :: LengthArray o n a

-- |Create a 'LengthArray' or runtime error if the assertion fails: @fromMaybe undefined . 'lengthArray'@
lengthArray' :: forall o n a . (KnownOrdering o, KnownNat n, HasLength a) => a -> LengthArray o n a
lengthArray' a = fromMaybe (error $ "lengthArray': fails check " ++ describeLengthArray (fromJust la)) la
  where la = lengthArray a

-- |Create a 'BoundedLengthArray' by trimming the given array if necessary.
boundLengthArray :: (KnownNat n, Array a) => a -> LengthArray 'LT n a
boundLengthArray a = l where
  l = LengthArray $ take (boundedLengthArrayBound l) a

-- |Create a 'BoundedLengthArray' by trimming the given array if necessary.
boundLengthArrayFromList :: (KnownNat n, Array a) => [Elem a] -> LengthArray 'LT n a
boundLengthArrayFromList a = l where
  l = LengthArray $ fromList $ take (boundedLengthArrayBound l) a

-- |Create a 'FixedLengthArray' by trimming or padding (on the right) as necessary.
padLengthArray :: (KnownNat n, Array a) => a -> Elem a -> LengthArray 'EQ n a
padLengthArray a p = l where
  a' = case compareLength a n of
    LT -> a <> replicate (n - length a) p
    EQ -> a
    GT -> take n a
  n = fixedLengthArrayLength l
  l = LengthArray a'

-- |Create a 'FixedLengthArray' filled with the same value.
constLengthArray :: (KnownNat n, Array a) => Elem a -> LengthArray 'EQ n a
constLengthArray p = l where
  l = LengthArray $ replicate (fixedLengthArrayLength l) p

instance (KnownOrdering o, KnownNat n, IsString a, HasLength a) => IsString (LengthArray o n a) where
  fromString s = fromMaybe
    (error $ "String " ++ show s ++ " fails LengthArray check " ++ describeLengthArray (fromJust ls))
    ls
    where ls = lengthArray $ fromString s

-- |An empty 'FixedLengthArray'.
emptyFixedLengthArray :: Array a => LengthArray 'EQ 0 a
emptyFixedLengthArray = LengthArray mempty

-- |An empty 'BoundedLengthArray'.
emptyBoundedLengthArray :: (CmpNat 0 n ~ 'LT, Array a) => LengthArray 'LT n a
emptyBoundedLengthArray = LengthArray mempty

-- |Grow the bound of a 'BoundedLengthArray'.
expandBoundedLengthArray :: (CmpNat n m ~ 'LT) => LengthArray 'LT n a -> LengthArray 'LT m a
expandBoundedLengthArray = LengthArray . unLengthArray

-- |Convert a 'FixedLengthArray' to a 'BoundedLengthArray'.
boundFixedLengthArray :: (CmpNat n m ~ 'LT) => LengthArray 'EQ n a -> LengthArray 'LT m a
boundFixedLengthArray = LengthArray . unLengthArray

-- |Append to two 'LengthArray's.
appendLengthArray :: Monoid a => LengthArray o n a -> LengthArray o m a -> LengthArray o (n + m) a
appendLengthArray (LengthArray a) (LengthArray b) = LengthArray $ mappend a b

fromLengthList :: Array a => LengthArray o n [Elem a] -> LengthArray o n a
fromLengthList = LengthArray . fromList . unLengthArray
