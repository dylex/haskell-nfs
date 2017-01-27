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

-- |Values that can be stored in an 'Network.ONCRPC.XDR.Types.Opaque' 'ByteString'.
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

toOpaque :: (Opaqued a, KnownOrdering o, KnownNat n) => a -> Maybe (LengthArray o n ByteString)
toOpaque = lengthArray . opacify

toOpaque' :: (Opaqued a, KnownOrdering o, KnownNat n) => a -> LengthArray o n ByteString
toOpaque' = lengthArray' . opacify

fromOpaque :: (Opaqued a, Monad m) => LengthArray o n ByteString -> m a
fromOpaque = unopacify . unLengthArray

fromOpaque' :: Opaqued a => LengthArray o n ByteString -> a
fromOpaque' = unopacify' . unLengthArray

instance Opaqued RPC.Authsys_parms
