module Network.ONCRPC.Client
  (
  ) where

import           Control.Concurrent (ThreadId, forkIO)
import           Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, withMVar, modifyMVarMasked)
import           Control.Monad (guard, when, unless)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.IntMap.Strict as IntMap
import           Data.Maybe (fromMaybe)
import qualified Network.Socket as Net
import qualified Network.Socket.All as NetAll
import qualified Network.Socket.ByteString as NetBS
import qualified Network.Socket.ByteString.Lazy as NetBSL
import           System.IO.Error (ioError, mkIOError, eofErrorType)
import           System.Random (randomIO)

import           Data.Serialize.Get.Feeder
import qualified Data.XDR as XDR
import           Network.ONCRPC.Types

data Request = Request
  { requestBody :: BSL.ByteString -- ^for retransmits
  , requestAction :: MVar BS.ByteString
  }

data State = State
  { stateSocket :: Net.Socket
  , stateXID :: XID
  , stateThread :: ThreadId
  , stateRequests :: IntMap.IntMap Request
  }

type Client = MVar State

clientThread :: Client -> Net.Socket -> IO ()
clientThread c sock@(Net.MkSocket _ _ Net.Stream _ _) = run where
  run = do
    r <- get Nothing
    return ()
  got = do
    return ()
  get f = do
    h <- maybe closed return =<< NetAll.recvStorable sock
    let (e, l) = unFragmentHeader h
        f' = fromMaybe (feeder got (l <$ guard e)) f
    (if e then return . fed else get . Just) =<< getBS l f'
  getBS n f = do
    b <- NetBS.recv sock n
    let l = BS.length b
        f' = feed f b
    if l < n
      then do
        when (l == 0) closed
        getBS (n - l) f'
      else return f'
  closed = ioError $ mkIOError eofErrorType "ONCRPC.Client: socket closed" Nothing Nothing
clientThread _ _ = fail "ONCRPC.Client: Unsupported socket type"

sendRequest :: State -> BSL.ByteString -> IO ()
sendRequest s@State{ stateSocket = sock@(Net.MkSocket _ _ Net.Stream _ _) } b = do
  NetAll.sendStorable sock $ mkFragmentHeader l (BSL.length h)
  NetBSL.sendAll sock h
  unless l $ sendRequest s t
  where
  (h, t) = BSL.splitAt maxFragmentSize b
  l = BSL.null t
sendRequest _ _ = fail "ONCRPC.Client: Unsupported socket type"

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

nextXID :: Client -> IO XID
nextXID cv = modifyMVarMasked cv $ \s@State{ stateXID = x } ->
  return (s{ stateXID = x + 1 }, x)

rpcCall :: (XDR.XDR a, XDR.XDR r) => Client -> Call a -> IO (Reply r)
rpcCall cv a = do
  xid <- nextXID cv
  withMVar cv $ flip sendRequest $ XDR.xdrSerializeLazy (xid, a)
  fail "TODO"

