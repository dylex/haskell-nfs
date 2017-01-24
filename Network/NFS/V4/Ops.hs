{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
-- {-# OPTIONS_GHC -ddump-splices #-}
module Network.NFS.V4.Ops
  ( NFSOp(..)
  ) where

import           Data.List ((\\))
import qualified Network.ONCRPC as RPC

import qualified Network.NFS.V4.Prot as NFS
import           Network.NFS.V4.Ops.TH

class (RPC.XDR a, RPC.XDR r) => NFSOp a r | r -> a, a -> r where
  nfsOpNum :: a -> NFS.Nfs_opnum4
  toNFSArgOp :: a -> NFS.Nfs_argop4
  fromNFSResOp :: NFS.Nfs_resop4 -> Maybe r
  nfsResOpStatus :: r -> NFS.Nfsstat4

thNFSOps $ enumFromTo minBound maxBound \\ [NFS.OP_SECINFO_NO_NAME]

newtype SECINFO_NO_NAME4res = SECINFO_NO_NAME4res{ sECINFO_NO_NAME4res :: NFS.SECINFO4res }
instance RPC.XDR SECINFO_NO_NAME4res where
  xdrType _ = "SECINFO_NO_NAME4res"
  xdrPut (SECINFO_NO_NAME4res s) = RPC.xdrPut s
  xdrGet = SECINFO_NO_NAME4res <$> RPC.xdrGet

instance NFSOp NFS.Secinfo_style4 SECINFO_NO_NAME4res where
  nfsOpNum _ = NFS.OP_SECINFO_NO_NAME
  toNFSArgOp = NFS.Nfs_argop4'OP_SECINFO_NO_NAME
  fromNFSResOp (NFS.Nfs_resop4'OP_SECINFO_NO_NAME r) = Just $ SECINFO_NO_NAME4res r
  fromNFSResOp _ = Nothing
  nfsResOpStatus = NFS.sECINFO4res'status . sECINFO_NO_NAME4res

