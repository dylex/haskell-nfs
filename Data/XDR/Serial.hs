-- |XDR Serialization

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Data.XDR.Serial
  ( XDR(..)
  , XDREnum(..)
  , xdrToEnum'
  , xdrPutEnum
  , xdrGetEnum
  , XDRUnion(..)
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
import           Data.Maybe (fromJust, isJust)
import           Data.Proxy (Proxy(..))
import qualified Data.Serialize as S
import qualified Data.Sext as Sext
import qualified Data.XDR.Types as XDR
-- import qualified Data.XDR.Specification as XDR
import           GHC.TypeLits (KnownNat, natVal)

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

-- |An XDR type defined with \"union\".
class XDR a => XDRUnion a where
  xdrDiscriminant :: a -> XDR.Int
  -- |Put the body of a union, without its discriminant.
  xdrPutUnionArm :: a -> S.Put
  -- |Get the body of a union based on its discriminant.
  xdrGetUnionArm :: XDR.Int -> S.Get a

-- |Default implementation of 'xdrPut' for 'XDRUnion'.
xdrPutUnion :: XDRUnion a => a -> S.Put
xdrPutUnion a = xdrPut (xdrDiscriminant a) >> xdrPutUnionArm a

-- |Default implementation of 'xdrGet' for 'XDRUnion'.
xdrGetUnion :: XDRUnion a => S.Get a
xdrGetUnion = xdrGet >>= xdrGetUnionArm

instance XDR a => XDR (XDR.Optional a) where
  xdrType = ('*':) . xdrType . fromJust
  xdrPut = xdrPutUnion
  xdrGet = xdrGetUnion

instance XDR a => XDRUnion (XDR.Optional a) where
  xdrDiscriminant = xdrFromEnum . isJust
  xdrPutUnionArm Nothing = return ()
  xdrPutUnionArm (Just a) = xdrPut a
  xdrGetUnionArm 0 = return Nothing
  xdrGetUnionArm 1 = Just <$> xdrGet
  xdrGetUnionArm _ = fail $ "xdrGetUnion: invalid discriminant for " ++ xdrType (undefined :: XDR.Optional a)

bsLength :: BS.ByteString -> XDR.Length
bsLength = fromIntegral . BS.length

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

xdrPutByteString :: XDR.Length -> BS.ByteString -> S.Put
xdrPutByteString l b = do
  unless (bsLength b == l) $ fail "xdrPutByteString: incorrect length"
  S.putByteString b
  xdrPutPad l

xdrPutByteStringLen :: XDR.Length -> BS.ByteString -> S.Put
xdrPutByteStringLen m b = do
  xdrPut l
  xdrPutByteString l b'
  where 
  b' = BS.take (fromIntegral m) b
  l = bsLength b'

xdrGetByteString :: XDR.Length -> S.Get BS.ByteString
xdrGetByteString l = do
  b <- S.getByteString $ fromIntegral l
  xdrGetPad l
  return b

xdrGetByteStringLen :: XDR.Length -> S.Get BS.ByteString
xdrGetByteStringLen m = do
  l <- xdrGet
  guard $ l <= m
  xdrGetByteString l

fixedLength :: KnownNat n => p n -> String -> String
fixedLength p = (++ ('[' : show (natVal p) ++ "]"))

variableLength :: KnownNat n => p n -> String -> String
variableLength p
  | n == XDR.maxLength = (++ "<>")
  | otherwise = (++ ('<' : show n ++ ">"))
  where n = fromIntegral $ natVal p

instance (KnownNat n, XDR a) => XDR (XDR.FixedArray n a) where
  xdrType = (fixedLength (Proxy :: Proxy n)) . xdrType . head . Sext.unwrap
  xdrPut a = do
    -- unless (length a' == fromInteger (natVal (Proxy :: Proxy n))) $ fail "xdrPutFixedArray: incorrect length"
    mapM_ xdrPut a'
    where
    a' = Sext.unwrap a
  xdrGet = Sext.unsafeCreate <$>
    replicateM (fromInteger (natVal (Proxy :: Proxy n))) xdrGet

instance (KnownNat n, XDR a) => XDR (XDR.Array n a) where
  xdrType (XDR.Array l) = variableLength (Proxy :: Proxy n) $ xdrType $ head l
  xdrPut (XDR.Array a) = do
    xdrPut (l :: XDR.Length)
    mapM_ xdrPut a'
    where
    m = fromInteger $ natVal (Proxy :: Proxy n)
    a' = take m a
    l = fromIntegral $ length a'
  xdrGet = XDR.Array <$> do
    l <- xdrGet :: S.Get XDR.Length
    guard $ l <= m
    replicateM (fromIntegral l) xdrGet
    where
    m = fromInteger $ natVal (Proxy :: Proxy n)

instance KnownNat n => XDR (XDR.FixedOpaque n) where
  xdrType _ = fixedLength (Proxy :: Proxy n) "opaque"
  xdrPut o =
    xdrPutByteString (fromInteger $ natVal (Proxy :: Proxy n)) $ Sext.unwrap o
  xdrGet = Sext.unsafeCreate <$>
    xdrGetByteString (fromInteger $ natVal (Proxy :: Proxy n))

instance KnownNat n => XDR (XDR.Opaque n) where
  xdrType o = variableLength o "opaque"
  xdrPut o@(XDR.Opaque b) =
    xdrPutByteString (fromInteger $ natVal o) b
  xdrGet = XDR.Opaque <$>
    xdrGetByteStringLen (fromInteger $ natVal (Proxy :: Proxy n))

instance KnownNat n => XDR (XDR.String n) where
  xdrType o = variableLength o "string"
  xdrPut s@(XDR.String b) =
    xdrPutByteStringLen (fromInteger $ natVal s) b
  xdrGet = XDR.String <$>
    xdrGetByteStringLen (fromInteger $ natVal (Proxy :: Proxy n))

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

xdrDeserialize :: (XDR a, Monad m) => BS.ByteString -> m a
xdrDeserialize = either fail return . S.runGet xdrGet

xdrDeserializeLazy :: (XDR a, Monad m) => BSL.ByteString -> m a
xdrDeserializeLazy = either fail return . S.runGetLazy xdrGet
