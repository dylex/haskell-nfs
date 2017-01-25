-- |ONC RPC Client interface.
-- Handles RPC client protocol layer.
-- Clients are fully thread-safe, allowing multiple outstanding requests, and automatically reconnect on error.
-- Currently error messages are just written to stdout.

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RecordWildCards #-}
module Network.ONCRPC.Client
  ( ClientServer(..)
  , Client
  , openClient
  , closeClient
  , clientCall
  , setClientAuth
  , rpcCall
  ) where

import           Control.Concurrent (ThreadId, forkIO, killThread, threadDelay)
import           Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar, modifyMVar, modifyMVar_, modifyMVarMasked)
import           Control.Exception (throwIO)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.IntMap.Strict as IntMap
import           Data.Time.Clock (getCurrentTime, diffUTCTime)
import qualified Network.Socket as Net
import           System.IO (hPutStrLn, stderr)
import           System.IO.Error (catchIOError)
import           System.Random (randomIO)

import qualified Network.ONCRPC.XDR as XDR
import qualified Network.ONCRPC.Prot as RPC
import           Network.ONCRPC.Types
import           Network.ONCRPC.Auth
import           Network.ONCRPC.Message
import           Network.ONCRPC.Transport

-- |How to connect to an RPC server.
-- Currently only TCP connections to pre-defined ports are supported (no portmap).
data ClientServer
  = ClientServerPort
    { clientServerHost :: Net.HostName -- ^Host name or IP address of server
    , clientServerPort :: Net.ServiceName -- ^Service name (not portmap) or port number
    } -- ^a known service by host/port, currently only TCP

data Request = forall a . XDR.XDR a => Request
  { requestBody :: BSL.ByteString -- ^for retransmits
  , requestAction :: MVar (Reply a)
  }

data State = State
  { stateSocket :: Maybe Net.Socket
  , stateXID :: XID
  , stateRequests :: IntMap.IntMap Request
  }

-- |An RPC Client.
data Client = Client
  { clientServer :: ClientServer
  , clientThread :: ThreadId
  , clientState :: MVar State
  , clientCred, clientVerf :: Auth
  }

warnMsg :: Show e => String -> e -> IO ()
warnMsg m = hPutStrLn stderr . (++) ("Network.ONCRPC.Client: " ++ m ++ ": ") . show

clientRecv :: Client -> Net.Socket -> IO ()
clientRecv c sock = next transportStart where
  next ms =
    check msg =<< recvGetFirst sock XDR.xdrGet ms
  msg (Right (RPC.Rpc_msg x (RPC.Rpc_msg_body'REPLY b))) ms = do
    q <- modifyMVarMasked (clientState c) $ \s@State{ stateRequests = m } -> do
      let (q, m') = IntMap.updateLookupWithKey (const $ const Nothing) (fromIntegral x) m
      return (s{ stateRequests = m' }, q)
    case q of
      Nothing -> do
        warnMsg "ignoring response to unknown xid" x
        next ms -- ignore
      Just (Request _ a) ->
        check (\r ms' -> do
          putMVar a $ either ReplyFail id r
          next ms')
          =<< recvGetNext sock (getReply b) ms
  msg e _ = warnMsg "couldn't decode reply msg" e -- return
  check _ Nothing = warnMsg "socket closed" () -- return
  check f (Just (r, ms)) = f r ms

clientConnect :: Client -> IO Net.Socket
clientConnect c = modifyMVar (clientState c) $ conn (clientServer c) where
  conn _ s@State{ stateSocket = Just sock } = return (s, sock)
  conn ClientServerPort{..} s = do
    addr:_ <- Net.getAddrInfo (Just Net.defaultHints{ Net.addrSocketType = Net.Stream }) (Just clientServerHost) (Just clientServerPort)
    sock <- Net.socket (Net.addrFamily addr) (Net.addrSocketType addr) (Net.addrProtocol addr)
    Net.connect sock (Net.addrAddress addr)
    resend sock (stateRequests s)
    return (s{ stateSocket = Just sock }, sock)
  resend sock = mapM_ $ sendTransport sock . requestBody

clientDisconnect :: Client -> IO ()
clientDisconnect c = modifyMVar_ (clientState c) $ \s -> do
  catchIOError
    (mapM_ Net.close $ stateSocket s)
    (warnMsg "close")
  return s{ stateSocket = Nothing }

clientMain :: Client -> IO ()
clientMain c = do
  t <- getCurrentTime
  catchIOError
    (clientConnect c >>= clientRecv c)
    (warnMsg "client")
  clientDisconnect c
  dt <- (`diffUTCTime` t) <$> getCurrentTime
  threadDelay $ ceiling $ 300000000 / (dt + 20)
  clientMain c

-- |Create a new RPC client to the given server.
-- This client must be destroyed with 'closeClient' when done.
openClient :: ClientServer -> IO Client
openClient srv = do
  s <- newEmptyMVar
  let c = Client
        { clientServer = srv
        , clientThread = error "clientThread"
        , clientState = s
        , clientCred = AuthNone
        , clientVerf = AuthNone
        }
  xid <- randomIO
  tid <- forkIO $ clientMain c
  putMVar s State
    { stateSocket = Nothing
    , stateXID = xid
    , stateRequests = IntMap.empty
    }
  return c{ clientThread = tid }

-- |Set the credentials and verifier to use when calling 'rpcCall' on a client.
-- Note that you can safely use different sets of credentials with the same underlying connection this way.
-- By default, both are set to 'AuthNone'.
setClientAuth :: Auth -> Auth -> Client -> Client
setClientAuth cred verf client = client
  { clientCred = cred
  , clientVerf = verf
  }

-- |Destroy an RPC client and close its underlying network connection.
-- Any outstanding requests return 'ReplyFail', any any further attempt to use the 'Client' may hang indefinitely.
closeClient :: Client -> IO ()
closeClient c = do
  killThread $ clientThread c
  clientDisconnect c
  -- Leave the state empty.
  s <- takeMVar $ clientState c
  mapM_ (\(Request _ a) -> putMVar a $ ReplyFail "closed") $ stateRequests s

-- |Send a call message using an open client, and wait for a reply, returning 'ReplyFail' on protocol error.
-- The request will be automatically retried until a response is received.
clientCall :: (XDR.XDR a, XDR.XDR r) => Client -> Call a r -> IO (Reply r)
clientCall c a = do
  rv <- newEmptyMVar
  p <- modifyMVar (clientState c) $ \s -> do
    let x = stateXID s
        q = Request
          { requestBody = XDR.xdrSerializeLazy $ MsgCall x a
          , requestAction = rv
          }
        (p, r) = IntMap.insertLookupWithKey (const const) (fromIntegral x) q (stateRequests s)
    catchIOError
      (mapM_ (`sendTransport` requestBody q) $ stateSocket s)
      (warnMsg "sendTransport")
    return (s{ stateRequests = r, stateXID = x+1 }, p)
  case p of
    Nothing -> return ()
    Just (Request _ v) -> putMVar v (ReplyFail "no response") -- should only happen on xid wraparound
  takeMVar rv

-- |Make an RPC request.
-- It waits for a response, retrying as necessary, or throws the 'Network.ONCRPC.Exception.RPCException', 'ReplyException', on any failure.
-- This uses the credentials set by 'setClientAuth'.
-- If you need to retrieve the auth verifier, use 'clientCall'.
rpcCall :: (XDR.XDR a, XDR.XDR r) => Client -> Procedure a r -> a -> IO r
rpcCall c p a = either throwIO return . replyResult
  =<< clientCall c (Call p (clientCred c) (clientVerf c) a)
