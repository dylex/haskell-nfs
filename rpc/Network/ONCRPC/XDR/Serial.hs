-- |XDR Serialization

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Network.ONCRPC.XDR.Serial
  ( XDR(..)
  , XDREnum(..)
  , xdrToEnum'
  , xdrPutEnum
  , xdrGetEnum
  , XDRUnion(..)
  , xdrDiscriminant
  , xdrPutUnion
  , xdrGetUnion
  
  , xdrSerialize
  , xdrSerializeLazy
  , xdrDeserialize
  , xdrDeserializeLazy
  ) where

import           Control.Monad (guard, unless, replicateM)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import           Data.Functor.Identity (runIdentity)
import           Data.Maybe (fromJust)
import           Data.Proxy (Proxy(..))
import qualified Data.Serialize as S
import qualified Data.Vector as V
import qualified Network.ONCRPC.XDR.Types as XDR
import           GHC.TypeLits (KnownNat, natVal)

import           Network.ONCRPC.XDR.Array

-- |An XDR type that can be (de)serialized.
class XDR a where
  -- |XDR identifier/type descriptor; argument value is ignored.
  xdrType :: a -> String
  xdrPut :: a -> S.Put
  xdrGet :: S.Get a

instance XDR XDR.Int where
  xdrType _ = "int"
  xdrPut = S.putInt32be
  xdrGet = S.getInt32be
instance XDR XDR.UnsignedInt where
  xdrType _ = "unsigned int"
  xdrPut = S.putWord32be
  xdrGet = S.getWord32be
instance XDR XDR.Hyper where
  xdrType _ = "hyper"
  xdrPut = S.putInt64be
  xdrGet = S.getInt64be
instance XDR XDR.UnsignedHyper where
  xdrType _ = "unsigned hyper"
  xdrPut = S.putWord64be
  xdrGet = S.getWord64be
instance XDR XDR.Float where
  xdrType _ = "float"
  xdrPut = S.putFloat32be
  xdrGet = S.getFloat32be
instance XDR XDR.Double where
  xdrType _ = "double"
  xdrPut = S.putFloat64be
  xdrGet = S.getFloat64be
instance XDR XDR.Bool where
  xdrType _ = "bool"
  xdrPut = xdrPutEnum
  xdrGet = xdrGetEnum

-- |An XDR type defined with \"enum\".
-- Note that the 'XDREnum' 'XDR.Int' value is not (necessarily) the same as the 'Enum' 'Int' value.
-- The 'Enum' instance is derived automatically to allow 'succ', etc. to work usefully in Haskell, whereas the 'XDREnum' reflects the XDR-defined values.
class (XDR a, Enum a) => XDREnum a where
  xdrFromEnum :: a -> XDR.Int
  xdrToEnum :: Monad m => XDR.Int -> m a

instance XDREnum XDR.Int where
  xdrFromEnum = id
  xdrToEnum = return

instance XDREnum XDR.UnsignedInt where
  xdrFromEnum = fromIntegral
  xdrToEnum = return . fromIntegral

-- |Version of 'xdrToEnum' that fails at runtime for invalid values: @fromMaybe undefined . 'xdrToEnum'@.
xdrToEnum' :: XDREnum a => XDR.Int -> a
xdrToEnum' = runIdentity . xdrToEnum

-- |Default implementation of 'xdrPut' for 'XDREnum'.
xdrPutEnum :: XDREnum a => a -> S.Put
xdrPutEnum = S.put . xdrFromEnum

-- |Default implementation of 'xdrGet' for 'XDREnum'.
xdrGetEnum :: XDREnum a => S.Get a
xdrGetEnum = xdrToEnum =<< S.get

instance XDREnum XDR.Bool where
  xdrFromEnum False = 0
  xdrFromEnum True = 1
  xdrToEnum 0 = return False
  xdrToEnum 1 = return True
  xdrToEnum _ = fail "invalid bool"

-- |An XDR type defined with \"union\"
class (XDR a, XDREnum (XDRDiscriminant a)) => XDRUnion a where
  type XDRDiscriminant a :: *
  -- |Split a union into its discriminant and body generator.
  xdrSplitUnion :: a -> (XDR.Int, S.Put)
  -- |Get the body of a union based on its discriminant.
  xdrGetUnionArm :: XDR.Int -> S.Get a

xdrDiscriminant :: XDRUnion a => a -> XDRDiscriminant a
xdrDiscriminant = xdrToEnum' . fst . xdrSplitUnion

-- |Default implementation of 'xdrPut' for 'XDRUnion'.
xdrPutUnion :: XDRUnion a => a -> S.Put
xdrPutUnion = uncurry ((>>) . xdrPut) . xdrSplitUnion

-- |Default implementation of 'xdrGet' for 'XDRUnion'.
xdrGetUnion :: XDRUnion a => S.Get a
xdrGetUnion = xdrGet >>= xdrGetUnionArm

instance XDR a => XDR (XDR.Optional a) where
  xdrType = ('*':) . xdrType . fromJust
  xdrPut = xdrPutUnion
  xdrGet = xdrGetUnion

instance XDR a => XDRUnion (XDR.Optional a) where
  type XDRDiscriminant (XDR.Optional a) = XDR.Bool
  xdrSplitUnion Nothing = (0, return ())
  xdrSplitUnion (Just a) = (1, xdrPut a)
  xdrGetUnionArm 0 = return Nothing
  xdrGetUnionArm 1 = Just <$> xdrGet
  xdrGetUnionArm _ = fail $ "xdrGetUnion: invalid discriminant for " ++ xdrType (undefined :: XDR.Optional a)

xdrPutPad :: XDR.Length -> S.Put
xdrPutPad n = case n `mod` 4 of
  0 -> return ()
  1 -> S.putWord16host 0 >> S.putWord8 0
  2 -> S.putWord16host 0
  ~3 -> S.putWord8 0

xdrGetPad :: XDR.Length -> S.Get ()
xdrGetPad n = case n `mod` 4 of
  0 -> return ()
  1 -> do
    0 <- S.getWord16host
    0 <- S.getWord8
    return ()
  2 -> do
    0 <- S.getWord16host
    return ()
  ~3 -> do
    0 <- S.getWord8
    return ()

bsLength :: BS.ByteString -> XDR.Length
bsLength = fromIntegral . BS.length

xdrPutByteString :: XDR.Length -> BS.ByteString -> S.Put
xdrPutByteString l b = do
  unless (bsLength b == l) $ fail "xdrPutByteString: incorrect length"
  S.putByteString b
  xdrPutPad l

xdrGetByteString :: XDR.Length -> S.Get BS.ByteString
xdrGetByteString l = do
  b <- S.getByteString $ fromIntegral l
  xdrGetPad l
  return b

fixedLength :: forall n a . KnownNat n => LengthArray 'EQ n a -> String -> String
fixedLength a = (++ ('[' : show (fixedLengthArrayLength a) ++ "]"))

variableLength :: forall n a . KnownNat n => LengthArray 'LT n a -> String -> String
variableLength a
  | n == XDR.maxLength = (++ "<>")
  | otherwise = (++ ('<' : show n ++ ">"))
  where n = fromIntegral $ boundedLengthArrayBound a

xdrGetBoundedArray :: forall n a . KnownNat n => (XDR.Length -> S.Get a) -> S.Get (LengthArray 'LT n a)
xdrGetBoundedArray g = do
  l <- xdrGet
  guard $ l <= fromIntegral (boundedLengthArrayBound (undefined :: LengthArray 'LT n a))
  unsafeLengthArray <$> g l

instance (KnownNat n, XDR a) => XDR (LengthArray 'EQ n [a]) where
  xdrType la = fixedLength la $ xdrType $ head $ unLengthArray la
  xdrPut la = do
    mapM_ xdrPut a
    where
    a = unLengthArray la
  xdrGet = unsafeLengthArray <$>
    replicateM (fromInteger (natVal (Proxy :: Proxy n))) xdrGet

instance (KnownNat n, XDR a) => XDR (LengthArray 'LT n [a]) where
  xdrType la = variableLength la $ xdrType $ head $ unLengthArray la
  xdrPut la = do
    xdrPut (fromIntegral (length a) :: XDR.Length)
    mapM_ xdrPut a
    where
    a = unLengthArray la
  xdrGet = xdrGetBoundedArray $ \l -> replicateM (fromIntegral l) xdrGet

instance (KnownNat n, XDR a) => XDR (LengthArray 'EQ n (V.Vector a)) where
  xdrType la = fixedLength la $ xdrType $ V.head $ unLengthArray la
  xdrPut la = do
    mapM_ xdrPut a
    where
    a = unLengthArray la
  xdrGet = unsafeLengthArray <$>
    V.replicateM (fromInteger (natVal (Proxy :: Proxy n))) xdrGet

instance (KnownNat n, XDR a) => XDR (LengthArray 'LT n (V.Vector a)) where
  xdrType la = variableLength la $ xdrType $ V.head $ unLengthArray la
  xdrPut la = do
    xdrPut (fromIntegral (V.length a) :: XDR.Length)
    mapM_ xdrPut a
    where
    a = unLengthArray la
  xdrGet = xdrGetBoundedArray $ \l -> V.replicateM (fromIntegral l) xdrGet

instance KnownNat n => XDR (LengthArray 'EQ n BS.ByteString) where
  xdrType o = fixedLength o "opaque"
  xdrPut o =
    xdrPutByteString (fromInteger $ natVal (Proxy :: Proxy n)) $ unLengthArray o
  xdrGet = unsafeLengthArray <$>
    xdrGetByteString (fromInteger $ natVal (Proxy :: Proxy n))

instance KnownNat n => XDR (LengthArray 'LT n BS.ByteString) where
  xdrType o = variableLength o "opaque"
  xdrPut o = do
    xdrPut l
    xdrPutByteString l b
    where 
    l = bsLength b
    b = unLengthArray o
  xdrGet = xdrGetBoundedArray xdrGetByteString

instance XDR () where
  xdrType () = "void"
  xdrPut () = return ()
  xdrGet = return ()

instance (XDR a, XDR b) => XDR (a, b) where
  xdrType (a, b) = xdrType a ++ '+' : xdrType b
  xdrPut (a, b) = xdrPut a >> xdrPut b
  xdrGet = (,) <$> xdrGet <*> xdrGet

instance (XDR a, XDR b, XDR c) => XDR (a, b, c) where
  xdrType (a, b, c) = xdrType a ++ '+' : xdrType b ++ '+' : xdrType c
  xdrPut (a, b, c) = xdrPut a >> xdrPut b >> xdrPut c
  xdrGet = (,,) <$> xdrGet <*> xdrGet <*> xdrGet

instance (XDR a, XDR b, XDR c, XDR d) => XDR (a, b, c, d) where
  xdrType (a, b, c, d) = xdrType a ++ '+' : xdrType b ++ '+' : xdrType c ++ '+' : xdrType d
  xdrPut (a, b, c, d) = xdrPut a >> xdrPut b >> xdrPut c >> xdrPut d
  xdrGet = (,,,) <$> xdrGet <*> xdrGet <*> xdrGet <*> xdrGet

xdrSerialize :: XDR a => a -> BS.ByteString
xdrSerialize = S.runPut . xdrPut

xdrSerializeLazy :: XDR a => a -> BSL.ByteString
xdrSerializeLazy = S.runPutLazy . xdrPut

-- |@"S.runGet' 'xdrGet'@
xdrDeserialize :: XDR a => BS.ByteString -> Either String a
xdrDeserialize = S.runGet xdrGet

-- |@"S.runGetLazy' 'xdrGet'@
xdrDeserializeLazy :: XDR a => BSL.ByteString -> Either String a
xdrDeserializeLazy = S.runGetLazy xdrGet
