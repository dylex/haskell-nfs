-- |Wrappers for "Network.Socket" calls that run over entire buffers or builds.
{-# LANGUAGE ScopedTypeVariables #-}
module Network.Socket.All
  ( recvAllBuf
  , recvStorable
  , sendAllBuf
  , sendStorable
  , sendBuilderWith
  , sendBuilder
  ) where

import           Control.Monad (unless)
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Builder.Extra as B
import           Data.Word (Word8)
import           Foreign.Ptr (Ptr, castPtr, plusPtr)
import           Foreign.Marshal (alloca, allocaBytes, with)
import           Foreign.Storable (Storable, sizeOf, peek)
import           Network.Socket
import qualified Network.Socket.ByteString as BS

allBufWith :: (Ptr a -> Int -> IO Int) -> Ptr a -> Int -> IO Int
allBufWith f p n = run 0 where
  run l
    | l >= n = return l
    | otherwise = do
      r <- f (p `plusPtr` l) (n - l)
      if r == 0
        then return l
        else run (l + r)

-- |Receive data from a socket, attempting to fill the entire buffer, blocking as necessary.
-- Any short read indicates a 0 (closed) result from 'recvBuf'.
recvAllBuf :: Socket -> Ptr Word8 -> Int -> IO Int
recvAllBuf = allBufWith . recvBuf

-- |Receive a raw memory object from a socket.
-- Returns 'Nothing' on short read.
recvStorable :: forall a . Storable a => Socket -> IO (Maybe a)
recvStorable s = alloca $ \p -> do
  r <- recvAllBuf s (castPtr p) n
  if r < n
    then return Nothing
    else Just <$> peek p
  where
  n = sizeOf (undefined :: a)

-- |Send an entire buffer to a socket, blocking as necessary.
-- Any short write indicates a 0 result from 'sendBuf'.
sendAllBuf :: Socket -> Ptr Word8 -> Int -> IO ()
sendAllBuf s p n = do
  r <- allBufWith (sendBuf s) p n
  unless (r == n) $ fail $ "sendAllBuf: sent " ++ show r ++ "/" ++ show n

-- |Send a raw memory object to a socket.
-- Returns 'False' on short read.
sendStorable :: Storable a => Socket -> a -> IO ()
sendStorable s x = with x $ \p ->
  sendAllBuf s (castPtr p) (sizeOf x)

-- |Effeciently send an entire builder to a network socket, using a specific buffer size.
-- Of course, this could be made even more efficient by using something like 'B.AllocationStrategy', but this is good enough for most purposes.
sendBuilderWith :: Socket -> Int -> B.Builder -> IO ()
sendBuilderWith s z0 = buf z0 . B.runBuilder where
  buf z w = do
    n <- allocaBytes z $ run z w
    case n of
      B.More z' w' -> buf z' w'
      ~B.Done -> return ()
  run z w p = do
    (l, n) <- w p z
    sendAllBuf s p l
    case n of
      B.More z' w' | z' <= z ->
        run z w' p
      B.Chunk b w' -> do
        BS.sendAll s b
        run z w' p
      _ -> return n

-- |Effeciently send an entire builder to a network socket, using a buffer size of 'B.defaultChunkSize'.
sendBuilder :: Socket -> B.Builder -> IO ()
sendBuilder s = sendBuilderWith s B.defaultChunkSize
