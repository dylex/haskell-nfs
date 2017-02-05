{-# LANGUAGE FlexibleInstances #-}
module Network.WebDAV.XML
  ( XMLTrees
  , XMLName
  , XMLConverter
  , XML(..)
  , xmlParser
  ) where

import           Control.Monad.Catch (MonadThrow)
import qualified Data.ByteString as BS
import qualified Data.Conduit as C
import qualified Data.Text as T
import qualified Data.XML.Types as XT
import qualified Network.URI as URI
import qualified Text.XML.Stream.Invertible as X
import qualified Text.XML.Stream.Parse as XP

type XMLTrees = [XT.Node]
type XMLName = XT.Name
type XMLConverter m a = X.Streamer m a

class XML a where
  xmlConvert :: MonadThrow m => XMLConverter m a

instance XML XT.Node where
  xmlConvert = X.passNode

instance XML T.Text where
  xmlConvert = X.content

instance XML String where
  xmlConvert = X.stringContent

instance XML URI.URI where
  xmlConvert = X.convert
    (maybe (Left "invalid anyURI") Right . URI.parseURIReference)
    (\u -> URI.uriToString id u "")
    X.stringContent

instance XML a => XML [a] where
  xmlConvert = X.manyI xmlConvert

xmlParser :: (XML a, MonadThrow m) => C.Sink BS.ByteString m a
xmlParser = XP.parseBytes XP.def C..| X.streamerParser "invalid XML document" xmlConvert
