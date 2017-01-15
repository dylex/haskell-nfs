{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Data.XDR.Serial
  ( XDR(..)
  , xdrIdentifier
  , XDREnum(..)
  , xdrToEnum
  , xdrPutEnum
  , xdrGetEnum
  , XDRUnion(..)
  , xdrPutUnion
  , xdrGetUnion
  )
  where

import           Control.Monad (guard, unless, replicateM)
import qualified Data.ByteString as BS
import           Data.Maybe (fromMaybe, fromJust)
import           Data.Proxy (Proxy(..))
import qualified Data.Serialize as B
import qualified Data.Sext as Sext
import qualified Data.XDR.Types as XDR
import qualified Data.XDR.Specification as XDR
import           GHC.TypeLits (KnownNat, natVal)

class XDR a where
  -- |Argument value is ignored
  xdrSpecification :: a -> XDR.Declaration
  xdrPut :: a -> B.Put
  xdrGet :: B.Get a

xdrIdentifier :: XDR a => a -> String
xdrIdentifier = XDR.identifierString . XDR.declarationIdentifier . xdrSpecification

instance XDR XDR.Int where
  xdrSpecification _ = XDR.Declaration (XDR.Identifier "int")            (XDR.TypeSingle XDR.TypeInt)
  xdrPut = B.putInt32be
  xdrGet = B.getInt32be
instance XDR XDR.UnsignedInt where
  xdrSpecification _ = XDR.Declaration (XDR.Identifier "unsigned int")   (XDR.TypeSingle XDR.TypeUnsignedInt)
  xdrPut = B.putWord32be
  xdrGet = B.getWord32be
instance XDR XDR.Hyper where
  xdrSpecification _ = XDR.Declaration (XDR.Identifier "hyper")          (XDR.TypeSingle XDR.TypeHyper)
  xdrPut = B.putInt64be
  xdrGet = B.getInt64be
instance XDR XDR.UnsignedHyper where
  xdrSpecification _ = XDR.Declaration (XDR.Identifier "unsigned hyper") (XDR.TypeSingle XDR.TypeUnsignedHyper)
  xdrPut = B.putWord64be
  xdrGet = B.getWord64be
instance XDR XDR.Float where
  xdrSpecification _ = XDR.Declaration (XDR.Identifier "float")          (XDR.TypeSingle XDR.TypeFloat)
  xdrPut = B.putFloat32be
  xdrGet = B.getFloat32be
instance XDR XDR.Double where
  xdrSpecification _ = XDR.Declaration (XDR.Identifier "double")         (XDR.TypeSingle XDR.TypeDouble)
  xdrPut = B.putFloat64be
  xdrGet = B.getFloat64be
instance XDR XDR.Bool where
  xdrSpecification _ = XDR.Declaration (XDR.Identifier "bool")           (XDR.TypeSingle XDR.TypeBool)
  xdrPut = xdrPutEnum
  xdrGet = xdrGetEnum

class (XDR a, Enum a) => XDREnum a where
  fromXDREnum :: a -> XDR.Int
  fromXDREnum = fromIntegral . fromEnum
  toXDREnum :: XDR.Int -> Maybe a

instance XDREnum XDR.Bool where
  fromXDREnum False = 0
  fromXDREnum True = 1
  toXDREnum 0 = Just False
  toXDREnum 1 = Just True
  toXDREnum _ = Nothing

xdrToEnum :: XDREnum a => Int -> a
xdrToEnum i = fromMaybe (error $ "invalid " ++ xdrIdentifier (fromJust x)) x
  where x = toXDREnum (fromIntegral i)

xdrPutEnum :: XDREnum a => a -> B.Put
xdrPutEnum = B.put . fromXDREnum

xdrGetEnum :: XDREnum a => B.Get a
xdrGetEnum = do
  i <- toXDREnum <$> B.get
  maybe (fail $ "invalid " ++ xdrIdentifier (fromJust i)) return i

class (XDR d, XDR a) => XDRUnion d a | a -> d where
  xdrUnionDiscriminant :: a -> d
  xdrPutUnionArm :: a -> B.Put
  xdrGetUnionArm :: d -> B.Get a

xdrPutUnion :: XDRUnion d a => a -> B.Put
xdrPutUnion a = xdrPut (xdrUnionDiscriminant a) >> xdrPutUnionArm a

xdrGetUnion :: XDRUnion d a => B.Get a
xdrGetUnion = xdrGet >>= xdrGetUnionArm

instance XDR a => XDR (XDR.Optional a) where
  xdrSpecification a = XDR.Declaration (XDR.Identifier $ '*':XDR.identifierString i) (XDR.TypeOptional (XDR.TypeIdentifier i)) where
    XDR.Declaration i _ = xdrSpecification (fromJust a)
  xdrPut = xdrPutUnion
  xdrGet = xdrGetUnion

instance XDR a => XDRUnion XDR.Bool (XDR.Optional a) where
  xdrUnionDiscriminant Nothing = False
  xdrUnionDiscriminant (Just _) = True
  xdrPutUnionArm Nothing = return ()
  xdrPutUnionArm (Just a) = xdrPut a
  xdrGetUnionArm False = return Nothing
  xdrGetUnionArm True = Just <$> xdrGet

bsLength :: BS.ByteString -> XDR.Length
bsLength = fromIntegral . BS.length

xdrPutPad :: XDR.Length -> B.Put
xdrPutPad n = case n `mod` 4 of
  0 -> return ()
  1 -> B.putWord16host 0 >> B.putWord8 0
  2 -> B.putWord16host 0
  ~3 -> B.putWord8 0

xdrGetPad :: XDR.Length -> B.Get ()
xdrGetPad n = case n `mod` 4 of
  0 -> return ()
  1 -> do
    0 <- B.getWord16host
    0 <- B.getWord8
    return ()
  2 -> do
    0 <- B.getWord16host
    return ()
  ~3 -> do
    0 <- B.getWord8
    return ()

xdrPutByteString :: XDR.Length -> BS.ByteString -> B.Put
xdrPutByteString l b = do
  unless (bsLength b == l) $ fail "xdrPutByteString: incorrect length"
  B.putByteString b
  xdrPutPad l

xdrPutByteStringLen :: XDR.Length -> BS.ByteString -> B.Put
xdrPutByteStringLen m b = do
  xdrPut l
  xdrPutByteString l b'
  where 
  b' = BS.take (fromIntegral m) b
  l = bsLength b'

xdrGetByteString :: XDR.Length -> B.Get BS.ByteString
xdrGetByteString l = do
  b <- B.getByteString $ fromIntegral l
  xdrGetPad l
  return b

xdrGetByteStringLen :: XDR.Length -> B.Get BS.ByteString
xdrGetByteStringLen m = do
  l <- xdrGet
  guard $ l <= m
  xdrGetByteString l

instance (KnownNat n, XDR a) => XDR (XDR.FixedArray n a) where
  xdrPut a = do
    -- unless (length a' == fromInteger (natVal (Proxy :: Proxy n))) $ fail "xdrPutFixedArray: incorrect length"
    mapM_ xdrPut a'
    where a' = Sext.unwrap a
  xdrGet = Sext.unsafeCreate <$>
    replicateM (fromInteger (natVal (Proxy :: Proxy n))) xdrGet

instance (KnownNat n, XDR a) => XDR (XDR.Array n a) where
  xdrPut (XDR.Array a) = do
    xdrPut (l :: XDR.Length)
    mapM_ xdrPut a'
    where
    m = fromInteger $ natVal (Proxy :: Proxy n)
    a' = take m a
    l = fromIntegral $ length a'
  xdrGet = XDR.Array <$> do
    l <- xdrGet :: B.Get XDR.Length
    guard $ l <= m
    replicateM (fromIntegral l) xdrGet
    where
    m = fromInteger $ natVal (Proxy :: Proxy n)

instance KnownNat n => XDR (XDR.FixedOpaque n) where
  xdrPut o =
    xdrPutByteString (fromInteger $ natVal (Proxy :: Proxy n)) $ Sext.unwrap o
  xdrGet = Sext.unsafeCreate <$>
    xdrGetByteString (fromInteger $ natVal (Proxy :: Proxy n))

instance KnownNat n => XDR (XDR.Opaque n) where
  xdrPut o@(XDR.Opaque b) =
    xdrPutByteString (fromInteger $ natVal o) b
  xdrGet = XDR.Opaque <$>
    xdrGetByteStringLen (fromInteger $ natVal (Proxy :: Proxy n))

instance KnownNat n => XDR (XDR.String n) where
  xdrPut s@(XDR.String b) =
    xdrPutByteStringLen (fromInteger $ natVal s) b
  xdrGet = XDR.String <$>
    xdrGetByteStringLen (fromInteger $ natVal (Proxy :: Proxy n))
