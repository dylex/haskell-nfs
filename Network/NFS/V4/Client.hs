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
  RPC.rpcCall' client $ RPC.Call (NFS.nFSPROC4_NULL procs) RPC.AuthNone RPC.AuthNone ()

nfsCall :: RPC.Client -> V.Vector NFS.Nfs_argop4 -> IO (V.Vector NFS.Nfs_resop4)
nfsCall client args = do
  NFS.COMPOUND4res stat _ res <- RPC.rpcCall' client $ RPC.Call (NFS.nFSPROC4_COMPOUND procs) RPC.AuthNone RPC.AuthNone
    $ NFS.COMPOUND4args emptyBoundedLengthArray NFS.nFS4_MINOR_VERS $ lengthArray' args
  if stat /= NFS.NFS4_OK
    then fail $ "NFS error: " ++ show stat
    else return $ unLengthArray res
