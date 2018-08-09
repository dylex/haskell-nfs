-- |Marshalling values into and out of 'Network.ONCRPC.XDR.Types.Opaque' byte strings.
-- Not really part of XDR, but convenient way to avoid many conversion functions.

{-# LANGUAGE DefaultSignatures #-}
module Network.ONCRPC.XDR.Opaque
  ( Opaqued(..)
  , unopacify'
  , toOpaque
  , toOpaque'
  , fromOpaque
  , fromOpaque'
  ) where

import           Data.ByteString (ByteString)
import           Data.Functor.Identity (runIdentity)

import           Network.ONCRPC.XDR.Array
import           Network.ONCRPC.XDR.Serial
import qualified Network.ONCRPC.Prot as RPC

-- |Values that can be stored in an 'Network.ONCRPC.XDR.Types.Opaque' 'OpaqueString' 'ByteString'.
-- The default implementation allows (re-)embedding of XDR-encoded data, such as with 'RPC.Opaque_auth'.
class Opaqued a where
  opacify :: a -> ByteString
  default opacify :: XDR a => a -> ByteString
  opacify = xdrSerialize
  unopacify :: Monad m => ByteString -> m a
  default unopacify :: (XDR a, Monad m) => ByteString -> m a
  unopacify = either fail return . xdrDeserialize

unopacify' :: Opaqued a => ByteString -> a
unopacify' = runIdentity . unopacify

toOpaque :: (Opaqued a, KnownOrdering o, KnownNat n) => a -> Maybe (LengthArray o n OpaqueString)
toOpaque = lengthArray . OpaqueString . opacify

toOpaque' :: (Opaqued a, KnownOrdering o, KnownNat n) => a -> LengthArray o n OpaqueString
toOpaque' = lengthArray' . OpaqueString . opacify

fromOpaque :: (Opaqued a, Monad m) => LengthArray o n OpaqueString -> m a
fromOpaque = unopacify . unOpaqueString . unLengthArray

fromOpaque' :: Opaqued a => LengthArray o n OpaqueString -> a
fromOpaque' = unopacify' . unOpaqueString . unLengthArray

instance Opaqued RPC.Authsys_parms
