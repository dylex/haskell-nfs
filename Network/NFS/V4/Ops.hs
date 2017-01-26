{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
-- {-# OPTIONS_GHC -ddump-splices #-}
module Network.NFS.V4.Ops
  ( NFSOp(..)
  , GETFH4args(..)
  , LOOKUPP4args(..)
  , PUTPUBFH4args(..)
  , PUTROOTFH4args(..)
  , READLINK4args(..)
  , RESTOREFH4args(..)
  , SAVEFH4args(..)
  , ILLEGAL4args(..)
  , SECINFO_NO_NAME4res(..)

  , NFSOps
  , nfsOpArgs
  , nfsOpHandler
  , nfsOp
  , nfsOp_
  ) where

import           Control.Exception (throw)
import           Data.Functor (void)
import           Data.Maybe (fromMaybe)
import           Data.Monoid ((<>))
import qualified Data.Vector as V
import qualified Network.ONCRPC as RPC

import qualified Network.NFS.V4.Prot as NFS
import           Network.NFS.V4.Exception
import           Network.NFS.V4.Ops.TH

class (RPC.XDR a, RPC.XDR r) => NFSOp a r | r -> a, a -> r where
  nfsOpNum :: a -> NFS.Nfs_opnum4
  toNFSOpArg :: a -> NFS.Nfs_argop4
  fromNFSOpRes :: NFS.Nfs_resop4 -> Maybe r

thNFSOps
  -- void args:
  [ NFS.OP_GETFH
  , NFS.OP_LOOKUPP
  , NFS.OP_PUTPUBFH
  , NFS.OP_PUTROOTFH
  , NFS.OP_READLINK
  , NFS.OP_RESTOREFH
  , NFS.OP_SAVEFH
  , NFS.OP_ILLEGAL
  ]
  -- exclude:
  [ NFS.OP_SECINFO_NO_NAME
  ]

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


-- |A set of compound operations and their handler.
data NFSOps a = NFSOps
  { nfsOpArgs :: V.Vector NFS.Nfs_argop4
  , nfsOpHandler :: V.Vector NFS.Nfs_resop4 -> a
  }

instance Functor NFSOps where
  fmap f (NFSOps a h) = NFSOps a (f . h)

-- |Operations are applied in the reverse order of application:
-- @f <*> a@ layers the @f@ ops on top of (i.e., after) the @a@ ops.
-- This better matches the state-transformation semantics of NFS4 compound operations.
instance Applicative NFSOps where
  pure x = NFSOps V.empty (const x)
  NFSOps fa fh <*> NFSOps xa xh = NFSOps (xa <> fa) $ \r ->
    fh (V.drop xn r) $ xh (V.take xn r)
    where xn = V.length xa

nfsOp :: NFSOp a r => a -> NFSOps r
nfsOp a = NFSOps (V.singleton $ toNFSOpArg a)
  $ fromMaybe (throw $ NFSException (Just $ nfsOpNum a) Nothing) . fromNFSOpRes . V.head

nfsOp_ :: NFSOp a r => a -> NFSOps ()
nfsOp_ = void . nfsOp
