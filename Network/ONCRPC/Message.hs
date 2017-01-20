module Network.ONCRPC.Message
  ( sendMessage
  , recvMessage
  , MessageState
  , initMessageState
  , messageIgnore
  , recvGet
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

data MessageState
  = MessageState
    { recordState :: RecordState
    , _bufferState :: BS.ByteString
    }
  | MessageIgnore
    { recordState :: RecordState
    }

initMessageState :: MessageState
initMessageState = MessageState RecordStart BS.empty

messageIgnore :: MessageState -> MessageState
messageIgnore = MessageIgnore . recordState

data RecvState a
  = RecvStart
    { recvState :: RecordState
    }
  | RecvGet
    { recvState :: RecordState
    , _recvGetter :: BS.ByteString -> S.Result a
    }
  | RecvIgnore
    { recvState :: RecordState
    }

recvGet :: Net.Socket -> S.Get a -> MessageState -> IO (Maybe (Either String a, MessageState))
recvGet sock getter = start where
  start (MessageState rs b)
    | BS.null b = get $ RecvStart rs
    | otherwise = got (RecvStart rs) b rs
  start (MessageIgnore rs) = get $ RecvIgnore rs
  get (RecvGet RecordStart f) = fed RecordStart $ f BS.empty
  get (RecvIgnore RecordStart) = get $ RecvStart RecordStart
  get s = uncurry (got s) =<< recvMessage sock (recvState s)
  got _ b _ | BS.null b = return Nothing
  got (RecvStart _) b rs = fed rs $ S.runGetChunk getter (recordRemaining rs) b
  got (RecvGet _ f) b rs = fed rs $ f b
  got (RecvIgnore _) _ rs = get $ RecvIgnore rs
  fed rs (S.Partial f) = get $ RecvGet rs f
  fed rs (S.Done r b) = return $ Just (Right r, MessageState rs b)
  fed rs (S.Fail e _) = return $ Just (Left e, MessageIgnore rs)
