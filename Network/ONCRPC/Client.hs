{-# LANGUAGE ExistentialQuantification #-}
module Network.ONCRPC.Client
  ( newClient
  , rpcCall
  ) where

import           Control.Concurrent (ThreadId, forkIO)
import           Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar, modifyMVar_, modifyMVarMasked)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.IntMap.Strict as IntMap
import qualified Network.Socket as Net
import           System.IO (hPutStrLn, stderr)
import           System.IO.Error (ioError, mkIOError, eofErrorType)
import           System.Random (randomIO)

import qualified Network.ONCRPC.XDR as XDR
import qualified Network.ONCRPC.Prot as RPC
import           Network.ONCRPC.Types
import           Network.ONCRPC.Message

data Request = forall a . XDR.XDR a => Request
  { requestBody :: BSL.ByteString -- ^for retransmits
  , requestAction :: MVar (Reply a) -- (Maybe (RPC.Reply_body, BSL.ByteString))
  }

data State = State
  { stateSocket :: Net.Socket
  , stateXID :: XID
  , stateThread :: ThreadId
  , stateRequests :: IntMap.IntMap Request
  }

type Client = MVar State

warn :: String -> IO ()
warn = hPutStrLn stderr

clientThread :: Client -> Net.Socket -> IO ()
clientThread cv sock = next messageStart where
  next ms = do
    maybe closed msg =<< recvGetFirst sock XDR.xdrGet ms
  msg (Right (RPC.Rpc_msg x (RPC.Rpc_msg_body'REPLY b)), ms) = do
    q <- modifyMVarMasked cv $ \s@State{ stateRequests = m } -> do
      let (q, m') = IntMap.updateLookupWithKey (const $ const Nothing) (fromIntegral x) m
      return (s{ stateRequests = m' }, q)
    case q of
      Nothing -> do
        warn $ "Response to unknown xid " ++ show x
        next ms
      Just (Request _ a) -> do
        (r, ms') <- maybe closed return =<< recvGetNext sock (getReply b) ms
        putMVar a $ either ReplyFail id r
        next $ ms'
  msg (e, ms) = do
    warn $ "Couldn't decode reply msg: " ++ show e
    next ms
  closed = ioError $ mkIOError eofErrorType "ONCRPC.Client: socket closed" Nothing Nothing

newClient :: Net.Socket -> IO Client
newClient sock = do
  c <- newEmptyMVar
  xid <- randomIO
  tid <- forkIO $ clientThread c sock
  putMVar c State
    { stateSocket = sock
    , stateXID = xid
    , stateThread = tid
    , stateRequests = IntMap.empty
    }
  return c

rpcCall :: (XDR.XDR a, XDR.XDR r) => Client -> Call a r -> IO (Reply r)
rpcCall cv a = do
  rv <- newEmptyMVar
  modifyMVar_ cv $ \s -> do
    let x = stateXID s
        q = Request
          { requestBody = XDR.xdrSerializeLazy $ MsgCall x a
          , requestAction = rv
          }
        (p, r) = IntMap.insertLookupWithKey (const const) (fromIntegral x) q (stateRequests s)
    case p of
      Nothing -> return ()
      Just (Request _ v) -> putMVar v (ReplyFail "no response") -- should only happen on xid wraparound
    sendMessage (stateSocket s) $ requestBody q
    return s{ stateRequests = r, stateXID = x+1 }
  takeMVar rv
