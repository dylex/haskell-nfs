module Network.ONCRPC.Transport
  ( sendTransport
  , recvTransport
  , TransportState
  , transportStart
  , recvGetFirst
  , recvGetNext
  ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Serialize.Get as S
import qualified Network.Socket as Net

import           Network.ONCRPC.RecordMarking

sendTransport :: Net.Socket -> BSL.ByteString -> IO ()
sendTransport sock@(Net.MkSocket _ _ Net.Stream _ _) = sendRecord sock
sendTransport _ = const $ fail "ONCRPC: Unsupported socket type"

recvTransport :: Net.Socket -> RecordState -> IO (BS.ByteString, RecordState)
recvTransport sock@(Net.MkSocket _ _ Net.Stream _ _) = recvRecord sock
recvTransport _ = const $ fail "ONCRPC: Unsupported socket type"

data TransportState = TransportState
  { _bufferState :: BS.ByteString
  , recordState :: RecordState
  }
  deriving (Eq, Show)

transportNext :: RecordState -> TransportState
transportNext = TransportState BS.empty

transportStart :: TransportState
transportStart = transportNext RecordStart

recvTransportWith :: Net.Socket -> RecordState -> (BS.ByteString -> RecordState -> IO (Maybe a)) -> IO (Maybe a)
recvTransportWith sock rs f = do
  (b, rs') <- recvTransport sock rs
  if BS.null b
    then return Nothing
    else f b rs'

-- |Get the next part of the current record, after calling 'recvGetFirst' to start.
recvGetNext :: Net.Socket -> S.Get a -> TransportState -> IO (Maybe (Either String a, TransportState))
recvGetNext sock getter = start where
  start (TransportState b rs) -- continue record
    | BS.null b = get Nothing rs -- check for more
    | otherwise = got Nothing b rs -- buffered data
  get f RecordStart = got f BS.empty RecordStart -- end of record
  get f rs = recvTransportWith sock rs $ got f -- read next block
  got Nothing b rs = fed rs $ S.runGetChunk getter (recordRemaining rs) b -- start parsing
  got (Just f) b rs = fed rs $ f b -- parse block
  fed rs (S.Partial f) = get (Just f) rs
  fed rs (S.Done r b) = return $ Just (Right r, TransportState b rs)
  fed rs (S.Fail e b) = return $ Just (Left e, TransportState b rs)

-- |Get the first part of the next record, possibly skipping over the rest of the current record.
recvGetFirst :: Net.Socket -> S.Get a -> TransportState -> IO (Maybe (Either String a, TransportState))
recvGetFirst sock getter = get . recordState where
  get rs = recvTransportWith sock rs $ got rs -- read next block
  got RecordStart b rs = recvGetNext sock getter $ TransportState b rs -- start next record
  got _ _ rs = get rs -- ignore remaining record
