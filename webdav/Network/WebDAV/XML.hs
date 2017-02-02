{-# LANGUAGE FlexibleInstances #-}
module Network.WebDAV.XML
  ( XMLTrees
  , XMLName
  , XMLConverter
  , XML(..)
  ) where

import qualified Data.Text as T
import qualified Data.XML.Types as XT
import qualified Network.URI as URI
import qualified Text.XML.Stream.Invertible as X

type XMLTrees = [XT.Node]
type XMLName = XT.Name
type XMLConverter a = X.Streamer Maybe a

class XML a where
  xmlConvert :: XMLConverter a

instance XML [XT.Node] where
  xmlConvert = X.passNodes

instance XML T.Text where
  xmlConvert = X.content

instance XML String where
  xmlConvert = X.stringContent

instance XML URI.URI where
  xmlConvert = X.convert
    (maybe (Left "invalid anyURI") Right . URI.parseURIReference)
    (\u -> URI.uriToString id u "")
    X.stringContent
