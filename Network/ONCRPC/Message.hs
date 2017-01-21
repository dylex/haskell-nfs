module Network.ONCRPC.Message
  ( sendMessage
  , recvMessage
  , MessageState
  , messageStart
  , recvGetFirst
  , recvGetNext
  ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Serialize.Get as S
import qualified Network.Socket as Net

import           Network.ONCRPC.RecordMarking

sendMessage :: Net.Socket -> BSL.ByteString -> IO ()
sendMessage sock@(Net.MkSocket _ _ Net.Stream _ _) = sendRecord sock
sendMessage _ = const $ fail "ONCRPC: Unsupported socket type"

recvMessage :: Net.Socket -> RecordState -> IO (BS.ByteString, RecordState)
recvMessage sock@(Net.MkSocket _ _ Net.Stream _ _) = recvRecord sock
recvMessage _ = const $ fail "ONCRPC: Unsupported socket type"

data MessageState = MessageState
  { _bufferState :: BS.ByteString
  , recordState :: RecordState
  }
  deriving (Eq, Show)

messageNext :: RecordState -> MessageState
messageNext = MessageState BS.empty

messageStart :: MessageState
messageStart = messageNext RecordStart

recvMessageWith :: Net.Socket -> RecordState -> (BS.ByteString -> RecordState -> IO (Maybe a)) -> IO (Maybe a)
recvMessageWith sock rs f = do
  (b, rs') <- recvMessage sock rs
  if BS.null b
    then return Nothing
    else f b rs'

-- |Get the next part of the current record, after calling 'recvGetFirst' to start.
recvGetNext :: Net.Socket -> S.Get a -> MessageState -> IO (Maybe (Either String a, MessageState))
recvGetNext sock getter = start where
  start (MessageState b rs) -- continue record
    | BS.null b = get Nothing rs -- check for more
    | otherwise = got Nothing b rs -- buffered data
  get f RecordStart = got f BS.empty RecordStart -- end of record
  get f rs = recvMessageWith sock rs $ got f -- read next block
  got Nothing b rs = fed rs $ S.runGetChunk getter (recordRemaining rs) b -- start parsing
  got (Just f) b rs = fed rs $ f b -- parse block
  fed rs (S.Partial f) = get (Just f) rs
  fed rs (S.Done r b) = return $ Just (Right r, MessageState b rs)
  fed rs (S.Fail e b) = return $ Just (Left e, MessageState b rs)

-- |Get the first part of the next record, possibly skipping over the rest of the current record.
recvGetFirst :: Net.Socket -> S.Get a -> MessageState -> IO (Maybe (Either String a, MessageState))
recvGetFirst sock getter = get . recordState where
  get rs = recvMessageWith sock rs $ got rs -- read next block
  got RecordStart b rs = recvGetNext sock getter $ MessageState b rs -- start next record
  got _ _ rs = get rs -- ignore remaining record
