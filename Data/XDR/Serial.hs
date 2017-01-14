{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Data.XDR.Serial
  where

import qualified Data.Serialize as B
import           Data.Maybe (fromMaybe, fromJust)
import qualified Data.XDR.Types as XDR
import qualified Data.XDR.Specification as XDR

class XDR a where
  -- |Argument value is ignored
  xdrSpecification :: a -> XDR.Declaration
  xdrPut :: a -> B.Put
  xdrGet :: B.Get a

xdrIdentifier :: XDR a => a -> XDR.Identifier
xdrIdentifier = XDR.declarationIdentifier . xdrSpecification

instance XDR XDR.Int where
  xdrSpecification _ = XDR.Declaration "int"            (XDR.TypeSingle XDR.TypeInt)
  xdrPut = B.putInt32be
  xdrGet = B.getInt32be
instance XDR XDR.UnsignedInt where
  xdrSpecification _ = XDR.Declaration "unsigned int"   (XDR.TypeSingle XDR.TypeUnsignedInt)
  xdrPut = B.putWord32be
  xdrGet = B.getWord32be
instance XDR XDR.Hyper where
  xdrSpecification _ = XDR.Declaration "hyper"          (XDR.TypeSingle XDR.TypeHyper)
  xdrPut = B.putInt64be
  xdrGet = B.getInt64be
instance XDR XDR.UnsignedHyper where
  xdrSpecification _ = XDR.Declaration "unsigned hyper" (XDR.TypeSingle XDR.TypeUnsignedHyper)
  xdrPut = B.putWord64be
  xdrGet = B.getWord64be
instance XDR XDR.Float where
  xdrSpecification _ = XDR.Declaration "float"          (XDR.TypeSingle XDR.TypeFloat)
  xdrPut = B.putFloat32be
  xdrGet = B.getFloat32be
instance XDR XDR.Double where
  xdrSpecification _ = XDR.Declaration "double"         (XDR.TypeSingle XDR.TypeDouble)
  xdrPut = B.putFloat64be
  xdrGet = B.getFloat64be
instance XDR XDR.Bool where
  xdrSpecification _ = XDR.Declaration "bool"           (XDR.TypeSingle XDR.TypeBool)
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
  xdrSpecification a = XDR.Declaration ('*':i) (XDR.TypeOptional (XDR.TypeIdentifier i)) where
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
