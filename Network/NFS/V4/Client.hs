module Network.NFS.V4.Client
  ( RPC.Client
  , openClient
  , nfsNullCall
  , nfsCall
  ) where

import qualified Data.Vector as V
import qualified Network.ONCRPC as RPC
import           Network.ONCRPC.XDR.Array (unLengthArray, lengthArray', emptyBoundedLengthArray)
import qualified Network.Socket as Net

import qualified Network.NFS.V4.Prot as NFS

openClient :: Net.HostName -> IO RPC.Client
openClient h = RPC.openClient $ RPC.ClientServerPort h "nfs"

procs :: NFS.NFS_V4
procs = NFS.nFS_V4 NFS.nFS4_PROGRAM

nfsNullCall :: RPC.Client -> IO ()
nfsNullCall client =
  RPC.rpcCall client (NFS.nFSPROC4_NULL procs) ()

nfsCall :: RPC.Client -> V.Vector NFS.Nfs_argop4 -> IO (NFS.Nfsstat4, V.Vector NFS.Nfs_resop4)
nfsCall client args = do
  NFS.COMPOUND4res stat _ res <- RPC.rpcCall client (NFS.nFSPROC4_COMPOUND procs)
    $ NFS.COMPOUND4args emptyBoundedLengthArray NFS.nFS4_MINOR_VERS $ lengthArray' args
  return (stat, unLengthArray res)
