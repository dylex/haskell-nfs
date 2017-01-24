-- |Exceptions for NFS calls

{-# LANGUAGE ExistentialQuantification #-}
module Network.NFS.V4.Exception
  ( NFSException(..)
  ) where

import           Control.Exception (Exception(..))
import           Data.Typeable (Typeable)
import           Network.ONCRPC.Exception (rpcExceptionToException, rpcExceptionFromException)

import qualified Network.NFS.V4.Prot as NFS

data NFSException = NFSException
  { nfsExceptionOpNum :: Maybe NFS.Nfs_opnum4
  , nfsExceptionStatus :: Maybe NFS.Nfsstat4
  } deriving (Typeable)

instance Show NFSException where
  show (NFSException op st) = "NFS Error: "
    ++ foldMap ((++ ": ") . show) op
    ++ maybe "invalid response" show st

instance Exception NFSException where
  toException = rpcExceptionToException
  fromException = rpcExceptionFromException

