module Network.NFS.V4.Client
  ( Client
  , openClient
  , closeClient
  , setClientAuth
  , nfsNullCall
  , nfsCall
  ) where

import           Control.Exception (throwIO)
import           Control.Monad (unless)
import qualified Data.Vector as V
import qualified Network.ONCRPC as RPC
import           Network.ONCRPC.XDR.Array (unLengthArray, lengthArray', emptyBoundedLengthArray)
import qualified Network.Socket as Net

import qualified Network.NFS.V4.Prot as NFS
import           Network.NFS.V4.Exception (NFSException(..))
import           Network.NFS.V4.Ops

data Client = Client
  { clientRPC :: RPC.Client
  , clientMinorVers :: RPC.UnsignedInt
  }

-- |See 'RPC.setClientAuth'.
setClientAuth :: RPC.Auth -> RPC.Auth -> Client -> Client
setClientAuth cred verf client = client
  { clientRPC = RPC.setClientAuth cred verf (clientRPC client) }

-- |See 'RPC.openClient'.
-- In addition, this negotiates a minor version and other client information with the server.
openClient :: Net.HostName -> IO Client
openClient h = do
  rpc <- RPC.openClient $ RPC.ClientServerPort h "nfs"
  let tryminor 0 = return 0
      tryminor m = do
        NFS.COMPOUND4res stat _ _ <- RPC.rpcCall rpc (NFS.nFSPROC4_COMPOUND procs)
          $ NFS.COMPOUND4args emptyBoundedLengthArray m emptyBoundedLengthArray
        case stat of
          NFS.NFS4ERR_MINOR_VERS_MISMATCH -> tryminor $ pred m
          NFS.NFS4_OK -> return m
          s -> throwIO $ NFSException Nothing (Just s)
  minor <- tryminor NFS.nFS4_MINOR_VERS
  return $ Client
    { clientRPC = rpc
    , clientMinorVers = minor
    }

-- |See 'RPC.closeClient'.
closeClient :: Client -> IO ()
closeClient = RPC.closeClient . clientRPC

procs :: NFS.NFS_V4
procs = NFS.nFS_V4 NFS.nFS4_PROGRAM

nfsNullCall :: RPC.Client -> IO ()
nfsNullCall client =
  RPC.rpcCall client (NFS.nFSPROC4_NULL procs) ()

-- |Generalize the various @res'status@ methods to 'NFS.Nfs_resop4'.
-- This perhaps should be somewhere else more general.
nfs_resop4'status :: NFS.Nfs_resop4 -> NFS.Nfsstat4 
nfs_resop4'status (NFS.Nfs_resop4'OP_ACCESS               r) = NFS.aCCESS4res'status r
nfs_resop4'status (NFS.Nfs_resop4'OP_CLOSE                r) = NFS.cLOSE4res'status r
nfs_resop4'status (NFS.Nfs_resop4'OP_COMMIT               r) = NFS.cOMMIT4res'status r
nfs_resop4'status (NFS.Nfs_resop4'OP_CREATE               r) = NFS.cREATE4res'status r
nfs_resop4'status (NFS.Nfs_resop4'OP_DELEGPURGE           r) = NFS.dELEGPURGE4res'status r
nfs_resop4'status (NFS.Nfs_resop4'OP_DELEGRETURN          r) = NFS.dELEGRETURN4res'status r
nfs_resop4'status (NFS.Nfs_resop4'OP_GETATTR              r) = NFS.gETATTR4res'status r
nfs_resop4'status (NFS.Nfs_resop4'OP_GETFH                r) = NFS.gETFH4res'status r
nfs_resop4'status (NFS.Nfs_resop4'OP_LINK                 r) = NFS.lINK4res'status r
nfs_resop4'status (NFS.Nfs_resop4'OP_LOCK                 r) = NFS.lOCK4res'status r
nfs_resop4'status (NFS.Nfs_resop4'OP_LOCKT                r) = NFS.lOCKT4res'status r
nfs_resop4'status (NFS.Nfs_resop4'OP_LOCKU                r) = NFS.lOCKU4res'status r
nfs_resop4'status (NFS.Nfs_resop4'OP_LOOKUP               r) = NFS.lOOKUP4res'status r
nfs_resop4'status (NFS.Nfs_resop4'OP_LOOKUPP              r) = NFS.lOOKUPP4res'status r
nfs_resop4'status (NFS.Nfs_resop4'OP_NVERIFY              r) = NFS.nVERIFY4res'status r
nfs_resop4'status (NFS.Nfs_resop4'OP_OPEN                 r) = NFS.oPEN4res'status r
nfs_resop4'status (NFS.Nfs_resop4'OP_OPENATTR             r) = NFS.oPENATTR4res'status r
nfs_resop4'status (NFS.Nfs_resop4'OP_OPEN_CONFIRM         r) = NFS.oPEN_CONFIRM4res'status r
nfs_resop4'status (NFS.Nfs_resop4'OP_OPEN_DOWNGRADE       r) = NFS.oPEN_DOWNGRADE4res'status r
nfs_resop4'status (NFS.Nfs_resop4'OP_PUTFH                r) = NFS.pUTFH4res'status r
nfs_resop4'status (NFS.Nfs_resop4'OP_PUTPUBFH             r) = NFS.pUTPUBFH4res'status r
nfs_resop4'status (NFS.Nfs_resop4'OP_PUTROOTFH            r) = NFS.pUTROOTFH4res'status r
nfs_resop4'status (NFS.Nfs_resop4'OP_READ                 r) = NFS.rEAD4res'status r
nfs_resop4'status (NFS.Nfs_resop4'OP_READDIR              r) = NFS.rEADDIR4res'status r
nfs_resop4'status (NFS.Nfs_resop4'OP_READLINK             r) = NFS.rEADLINK4res'status r
nfs_resop4'status (NFS.Nfs_resop4'OP_REMOVE               r) = NFS.rEMOVE4res'status r
nfs_resop4'status (NFS.Nfs_resop4'OP_RENAME               r) = NFS.rENAME4res'status r
nfs_resop4'status (NFS.Nfs_resop4'OP_RENEW                r) = NFS.rENEW4res'status r
nfs_resop4'status (NFS.Nfs_resop4'OP_RESTOREFH            r) = NFS.rESTOREFH4res'status r
nfs_resop4'status (NFS.Nfs_resop4'OP_SAVEFH               r) = NFS.sAVEFH4res'status r
nfs_resop4'status (NFS.Nfs_resop4'OP_SECINFO              r) = NFS.sECINFO4res'status r
nfs_resop4'status (NFS.Nfs_resop4'OP_SETATTR              r) = NFS.sETATTR4res'status r
nfs_resop4'status (NFS.Nfs_resop4'OP_SETCLIENTID          r) = NFS.sETCLIENTID4res'status r
nfs_resop4'status (NFS.Nfs_resop4'OP_SETCLIENTID_CONFIRM  r) = NFS.sETCLIENTID_CONFIRM4res'status r
nfs_resop4'status (NFS.Nfs_resop4'OP_VERIFY               r) = NFS.vERIFY4res'status r
nfs_resop4'status (NFS.Nfs_resop4'OP_WRITE                r) = NFS.wRITE4res'status r
nfs_resop4'status (NFS.Nfs_resop4'OP_RELEASE_LOCKOWNER    r) = NFS.rELEASE_LOCKOWNER4res'status r
nfs_resop4'status (NFS.Nfs_resop4'OP_BACKCHANNEL_CTL      r) = NFS.bACKCHANNEL_CTL4res'bcr_status r
nfs_resop4'status (NFS.Nfs_resop4'OP_BIND_CONN_TO_SESSION r) = NFS.bIND_CONN_TO_SESSION4res'bctsr_status r
nfs_resop4'status (NFS.Nfs_resop4'OP_EXCHANGE_ID          r) = NFS.eXCHANGE_ID4res'eir_status r
nfs_resop4'status (NFS.Nfs_resop4'OP_CREATE_SESSION       r) = NFS.cREATE_SESSION4res'csr_status r
nfs_resop4'status (NFS.Nfs_resop4'OP_DESTROY_SESSION      r) = NFS.dESTROY_SESSION4res'dsr_status r
nfs_resop4'status (NFS.Nfs_resop4'OP_FREE_STATEID         r) = NFS.fREE_STATEID4res'fsr_status r
nfs_resop4'status (NFS.Nfs_resop4'OP_GET_DIR_DELEGATION   r) = NFS.gET_DIR_DELEGATION4res'gddr_status r
nfs_resop4'status (NFS.Nfs_resop4'OP_GETDEVICEINFO        r) = NFS.gETDEVICEINFO4res'gdir_status r
nfs_resop4'status (NFS.Nfs_resop4'OP_GETDEVICELIST        r) = NFS.gETDEVICELIST4res'gdlr_status r
nfs_resop4'status (NFS.Nfs_resop4'OP_LAYOUTCOMMIT         r) = NFS.lAYOUTCOMMIT4res'locr_status r
nfs_resop4'status (NFS.Nfs_resop4'OP_LAYOUTGET            r) = NFS.lAYOUTGET4res'logr_status r
nfs_resop4'status (NFS.Nfs_resop4'OP_LAYOUTRETURN         r) = NFS.lAYOUTRETURN4res'lorr_status r
nfs_resop4'status (NFS.Nfs_resop4'OP_SECINFO_NO_NAME      r) = NFS.sECINFO4res'status r
nfs_resop4'status (NFS.Nfs_resop4'OP_SEQUENCE             r) = NFS.sEQUENCE4res'sr_status r
nfs_resop4'status (NFS.Nfs_resop4'OP_SET_SSV              r) = NFS.sET_SSV4res'ssr_status r
nfs_resop4'status (NFS.Nfs_resop4'OP_TEST_STATEID         r) = NFS.tEST_STATEID4res'tsr_status r
nfs_resop4'status (NFS.Nfs_resop4'OP_WANT_DELEGATION      r) = NFS.wANT_DELEGATION4res'wdr_status r
nfs_resop4'status (NFS.Nfs_resop4'OP_DESTROY_CLIENTID     r) = NFS.dESTROY_CLIENTID4res'dcr_status r
nfs_resop4'status (NFS.Nfs_resop4'OP_RECLAIM_COMPLETE     r) = NFS.rECLAIM_COMPLETE4res'rcr_status r
nfs_resop4'status (NFS.Nfs_resop4'OP_ILLEGAL              r) = NFS.iLLEGAL4res'status r

-- |Make a compound NFS call, returning a vector of results, or throwing an 'NFSException' on any NFS error or result mismatch.
nfsCall :: Client -> Ops a -> IO a
nfsCall client ops = do
  NFS.COMPOUND4res stat _ lres <- RPC.rpcCall (clientRPC client) (NFS.nFSPROC4_COMPOUND procs)
    $ NFS.COMPOUND4args emptyBoundedLengthArray (clientMinorVers client) $ lengthArray' arg
  let res = unLengthArray lres
  mapM_ (\r -> chkerr (Just $ NFS.nfs_resop4'resop r) $ nfs_resop4'status r) res
  chkerr Nothing stat
  V.zipWithM_ (\a r -> unless (NFS.nfs_argop4'argop a == NFS.nfs_resop4'resop r)
      $ throwIO $ NFSException (Just $ NFS.nfs_argop4'argop a) Nothing)
    arg res
  case compare (V.length res) (V.length arg) of
    EQ -> return $ opHandler ops res
    LT -> bad $ NFS.nfs_argop4'argop $ arg V.! V.length res
    GT -> bad $ NFS.nfs_resop4'resop $ res V.! V.length arg
  where
  arg = opArgs ops
  chkerr _ NFS.NFS4_OK = return ()
  chkerr o s = throwIO $ NFSException o (Just s)
  bad o = throwIO $ NFSException (Just o) Nothing
