module Network.ONCRPC.RecordMarking
  ( sendRecord
  , RecordState(RecordStart)
  , recordDone
  , recordRemaining
  , recvRecord
  ) where

import           Control.Monad (unless)
import           Data.Bits (Bits, finiteBitSize, bit, clearBit, setBit, testBit)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import           Data.Word (Word32)
import qualified Network.Socket as Net
import qualified Network.Socket.All as NetAll
import qualified Network.Socket.ByteString as NetBS
import qualified Network.Socket.ByteString.Lazy as NetBSL

-- |A raw RPC record fragment header, stored in network byte order.
type FragmentHeader = Word32

fragmentHeaderBit :: Int
fragmentHeaderBit = pred $ finiteBitSize (0 :: FragmentHeader)

maxFragmentSize :: (Bits i, Integral i) => i
maxFragmentSize = pred $ bit fragmentHeaderBit

unFragmentHeader :: Integral i => FragmentHeader -> (Bool, i)
unFragmentHeader w =
  (testBit w' fragmentHeaderBit, fromIntegral $ clearBit w' fragmentHeaderBit)
  where w' = Net.ntohl w

mkFragmentHeader :: Integral i => Bool -> i -> FragmentHeader
mkFragmentHeader l n = Net.htonl $ sb l $ fromIntegral n where
  sb True x = setBit x fragmentHeaderBit
  sb False x = x

sendRecord :: Net.Socket -> BSL.ByteString -> IO ()
sendRecord sock b = do
  NetAll.sendStorable sock $ mkFragmentHeader l (BSL.length h)
  NetBSL.sendAll sock h
  unless l $ sendRecord sock t
  where
  (h, t) = BSL.splitAt maxFragmentSize b
  l = BSL.null t

data RecordState
  = RecordStart
  | RecordHeader
  | RecordFragment
    { _fragmentLast :: !Bool
    , _fragmentLength :: !Int
    }
  deriving (Eq, Show)

-- |Is the current record complete?
recordDone :: RecordState -> Bool
recordDone RecordStart = True
recordDone _ = False

-- |How many bytes are left in this record, if known?
recordRemaining :: RecordState -> Maybe Int
recordRemaining RecordStart = Just 0
recordRemaining (RecordFragment True n) = Just n
recordRemaining _ = Nothing

-- |Receive the next block of a record
recvRecord :: Net.Socket -> RecordState -> IO (BS.ByteString, RecordState)
recvRecord sock (RecordFragment e n) = do
  b <- NetBS.recv sock n
  let l = BS.length b
  return (b, if l < n
    then RecordFragment e (n - l)
    else if e
      then RecordStart
      else RecordHeader)
recvRecord sock s =
  maybe (return (BS.empty, s)) (recvRecord sock . uncurry RecordFragment . unFragmentHeader) =<< NetAll.recvStorable sock
