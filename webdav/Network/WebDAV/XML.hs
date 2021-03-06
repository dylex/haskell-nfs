{-# LANGUAGE FlexibleInstances #-}
module Network.WebDAV.XML
  ( XMLTrees
  , XMLName
  , XMLConverter
  , XMLSource
  , XML(..)
  , xmlParser
  , xmlRender
  , xmlSource
  ) where

import           Control.Monad.Catch (MonadThrow)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.Conduit as C
import qualified Data.Text as T
import qualified Data.XML.Types as XT
import qualified Network.URI as URI
import qualified Text.XML.Stream.Invertible as X
import qualified Text.XML.Stream.Parse as XP
import qualified Text.XML.Stream.Render as XR

type XMLTrees = [XT.Node]
type XMLName = XT.Name
type XMLConverter m a = X.Streamer m a
type XMLSource m = C.Source m XT.Event

class XML a where
  xmlConvert :: MonadThrow m => XMLConverter m a

instance XML () where
  xmlConvert = X.unit

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

instance {-# OVERLAPPABLE #-} XML a => XML [a] where
  xmlConvert = X.manyI xmlConvert

xmlParser :: (XML a, MonadThrow m) => C.Sink BS.ByteString m a
xmlParser = XP.parseBytes XP.def C..| X.streamerParser "invalid XML document" xmlConvert

xmlRender :: Monad m => C.Conduit XT.Event m BSB.Builder
xmlRender = (C.yield XT.EventBeginDocument >> C.awaitForever C.yield)
  C..| XR.renderBuilder XR.def

xmlSource :: (XML a, MonadThrow m) => a -> XMLSource m
xmlSource = X.streamerRender xmlConvert
