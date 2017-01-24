{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
-- {-# OPTIONS_GHC -ddump-splices #-}
module Network.NFS.V4.Ops
  ( NFSOp(..)
  ) where

import           Control.Exception (evaluate, throw, throwIO)
import           Control.Monad (join)
import           Data.Foldable (fold)
import           Data.List ((\\))
import qualified Data.Vector as V
import qualified Network.ONCRPC as RPC

import qualified Network.NFS.V4.Prot as NFS
import           Network.NFS.V4.Exception
import           Network.NFS.V4.Ops.TH
import           Network.NFS.V4.Client (Client, nfsCall)

class (RPC.XDR a, RPC.XDR r) => NFSOp a r | r -> a, a -> r where
  nfsOpNum :: a -> NFS.Nfs_opnum4
  toNFSOpArg :: a -> NFS.Nfs_argop4
  fromNFSOpRes :: NFS.Nfs_resop4 -> Maybe r
  nfsOpResStatus :: r -> NFS.Nfsstat4

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
  nfsOpResStatus = NFS.sECINFO4res'status . sECINFO_NO_NAME4res


data NFSOpItem w = NFSOpItem
  { nfsOpItemArg :: NFS.Nfs_argop4
  , nfsOpItemHandler :: NFS.Nfs_resop4 -> w
  }

nfsOp :: NFSOp a r => a -> (r -> w) -> NFSOpItem w
nfsOp a f = NFSOpItem (toNFSOpArg a)
  $ maybe (err Nothing) (join (chk . nfsOpResStatus)) . fromNFSOpRes
  where
  o = nfsOpNum a
  err = throw . NFSException (Just o)
  chk NFS.NFS4_OK r = f r
  chk s _ = err $ Just s

nfsOpCall :: Monoid w => RPC.Client -> [NFSOpItem w] -> IO w
nfsOpCall client ops = do
  (s, r) <- nfsCall client $ V.fromList $ map nfsOpItemArg ops
  w <- evaluate $ fold $ zipWith nfsOpItemHandler ops $ V.toList r
  case s of
    NFS.NFS4_OK -> return w
    _ -> throwIO $ NFSException Nothing $ Just s
