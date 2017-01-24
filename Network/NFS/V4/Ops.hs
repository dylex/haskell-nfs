{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
-- {-# OPTIONS_GHC -ddump-splices #-}
module Network.NFS.V4.Ops
  ( NFSOp(..)
  , nfsOp
  , nfsOp_
  , nfsOpCall
  ) where

import           Control.Exception (throw)
import           Data.List ((\\))
import           Data.Maybe (fromMaybe)
import qualified Data.Vector as V
import qualified Network.ONCRPC as RPC

import qualified Network.NFS.V4.Prot as NFS
import           Network.NFS.V4.Exception
import           Network.NFS.V4.Ops.TH
import           Network.NFS.V4.Client (nfsCall)

class (RPC.XDR a, RPC.XDR r) => NFSOp a r | r -> a, a -> r where
  nfsOpNum :: a -> NFS.Nfs_opnum4
  toNFSOpArg :: a -> NFS.Nfs_argop4
  fromNFSOpRes :: NFS.Nfs_resop4 -> Maybe r

thNFSOps $ enumFromTo minBound maxBound \\ [NFS.OP_SECINFO_NO_NAME]

newtype SECINFO_NO_NAME4res = SECINFO_NO_NAME4res{ sECINFO_NO_NAME4res :: NFS.SECINFO4res }
instance RPC.XDR SECINFO_NO_NAME4res where
  xdrType _ = "SECINFO_NO_NAME4res"
  xdrPut (SECINFO_NO_NAME4res s) = RPC.xdrPut s
  xdrGet = SECINFO_NO_NAME4res <$> RPC.xdrGet

instance NFSOp NFS.Secinfo_style4 SECINFO_NO_NAME4res where
  nfsOpNum _ = NFS.OP_SECINFO_NO_NAME
  toNFSOpArg = NFS.Nfs_argop4'OP_SECINFO_NO_NAME
  fromNFSOpRes (NFS.Nfs_resop4'OP_SECINFO_NO_NAME r) = Just $ SECINFO_NO_NAME4res r
  fromNFSOpRes _ = Nothing


data NFSOpCall a
  = NFSOpNull a
  | forall b . NFSOpThen
    { nfsOpInit :: NFSOpCall b
    , nfsOpArg :: NFS.Nfs_argop4
    , nfsOpHandler :: NFS.Nfs_resop4 -> b -> a
    }

instance Functor NFSOpCall where
  fmap f (NFSOpNull a) = NFSOpNull (f a)
  fmap f (NFSOpThen i a h) = NFSOpThen i a ((f .) . h)

-- |Ops are applied in reverse order.
-- This (aka, @**@) seems to be easier to implement than <*>
nfsOpPair :: NFSOpCall a -> NFSOpCall b -> NFSOpCall (a, b)
nfsOpPair (NFSOpNull a) (NFSOpNull b) =
  NFSOpNull (a, b)
nfsOpPair (NFSOpNull a) (NFSOpThen bi ba bh) =
  NFSOpThen bi ba (\r b -> (a, bh r b))
nfsOpPair (NFSOpThen ai aa ah) bc =
  NFSOpThen (nfsOpPair ai bc) aa (\r (a, b) -> (ah r a, b))

-- |Note that the resulting order is the reverse of what you might expect:
-- @f <*> a@ layers the @f@ ops on top of (i.e., after) the @a@ ops.
instance Applicative NFSOpCall where
  pure = NFSOpNull
  NFSOpNull f <*> x = fmap f x
  f <*> NFSOpNull x = fmap ($ x) f
  NFSOpThen fi fa fh <*> x = NFSOpThen (nfsOpPair fi x) fa (uncurry . fh)
  NFSOpNull _ *> b = b
  NFSOpThen ai aa _ *> b = NFSOpThen (ai *> b) aa (const id)
  NFSOpNull a <* b = a <$ b
  NFSOpThen ai aa ah <* b = NFSOpThen (ai <* b) aa ah

nfsOp :: NFSOp a r => a -> (r -> b -> c) -> NFSOpCall b -> NFSOpCall c
nfsOp a f i = NFSOpThen i (toNFSOpArg a)
  $ f . fromMaybe (throw $ NFSException (Just $ nfsOpNum a) Nothing) . fromNFSOpRes

nfsOp_ :: NFSOp a r => a -> NFSOpCall b -> NFSOpCall b
nfsOp_ a i = NFSOpThen i (toNFSOpArg a) $ const id

nfsOpCallArgs :: NFSOpCall a -> V.Vector NFS.Nfs_argop4
nfsOpCallArgs (NFSOpNull _) = V.empty
nfsOpCallArgs (NFSOpThen i a _) = nfsOpCallArgs i `V.snoc` a

nfsOpCallProc :: NFSOpCall a -> V.Vector NFS.Nfs_resop4 -> a
nfsOpCallProc (NFSOpNull r) _ = r
nfsOpCallProc (NFSOpThen i _ h) v = h (V.last v) $ nfsOpCallProc i (V.init v)

nfsOpCall :: RPC.Client -> NFSOpCall a -> IO a
nfsOpCall client ops = do
  r <- nfsCall client $ nfsOpCallArgs ops
  return $ nfsOpCallProc ops r
