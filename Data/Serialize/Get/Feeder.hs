module Data.Serialize.Get.Feeder
  ( Feeder
  , feeder
  , feed
  , fed
  ) where

import qualified Data.ByteString as BS
import           Data.Serialize.Get

-- |A convenient way to manage incremental 'Get'
type Feeder a = Result a

-- |Create a new feeder based on the given Get.  See 'runGetChunk'.
feeder :: Get a -> Maybe Int -> Feeder a
feeder g n = Partial $ \b -> runGetChunk g (subtract (BS.length b) <$> n) b

feed' :: Feeder a -> BS.ByteString -> Feeder a
feed' (Fail e b) = Fail e . BS.append b
feed' (Partial f) = f
feed' (Done r b) = Done r . BS.append b

-- |Pass another chunk to a 'Feeder'.
feed :: Feeder a -> BS.ByteString -> Feeder a
feed f b
  | BS.null b = f
  | otherwise = feed' f b

fed' :: Feeder a -> Either String a
fed' (Fail e _) = Left e
fed' (Partial _) = Left "feedResult: Failed reading: Internal error: unexpected Partial."
fed' (Done r _) = Right r

-- |Get the final result of a 'Feeder'.
-- Like 'runGet', this ignores any unused input.
fed :: Feeder a -> Either String a
fed (Partial f) = fed' $ f BS.empty
fed r = fed' r
