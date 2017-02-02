{-# OPTIONS_GHC -fno-warn-orphans #-}
module Network.WebDAV.XML
  ( module Text.XML.HXT.Arrow.Pickle.Xml.Invertible
  , xpTrimAnyElem
  , xpTrimAnyElems
  , xpTrimElemNS
  , xpTrimNameElem
  , xpURI
  , toXMLDoc
  ) where

import           Control.Invertible.Monoidal
import           Control.Monad.State.Class (gets)
import qualified Data.ByteString.Lazy as BSL
import qualified Network.URI as URI
import           Text.XML.HXT.Arrow.Pickle.Schema (Schema(Any), scDTxsd)
import           Text.XML.HXT.Arrow.Pickle.Xml.Invertible
import qualified Text.XML.HXT.Core as HXT
import qualified Text.XML.HXT.DOM.XmlNode as XN
import qualified Text.XML.HXT.XMLSchema.DataTypeLibW3CNames as XSD

xpTrimAnyElem :: PU HXT.XmlTree
xpTrimAnyElem = xpTrim xpAnyElem

xpTrimAnyElems :: PU HXT.XmlTrees
xpTrimAnyElems = xpList xpTrimAnyElem

xpTrimElemNS :: String -> String -> String -> PU a -> PU a
xpTrimElemNS s p n c = xpWhitespace *< xpElemNS s p n c >* xpWhitespace

xpTrimNameElem :: PU a -> PU (HXT.QName, a)
xpTrimNameElem p = xpWhitespace *< PU
  { appPickle = \(n, c) ->
      let s = appPickle p c emptySt in
      putCont $ XN.NTree (HXT.XTag n $ attributes s) (contents s)
  , appUnPickle = do
      l <- gets nesting
      e <- getCont
      n <- liftMaybe "xpNameElem: XML element expected" $ XN.getElemName e
      liftUnpickleVal $ (,) n <$> unpickleElem' (xpCheckEmpty p) (succ l) e
  , theSchema = Any
  }

xpURI :: PU URI.URI
xpURI = xpWrapEither 
  ( maybe (Left "invalid anyURI") Right . URI.parseURIReference
  , \u -> URI.uriToString id u "")
  $ xpText0DT $ scDTxsd XSD.xsd_anyURI []

toXMLDoc :: XmlPickler a => a -> BSL.ByteString
toXMLDoc = BSL.concat
  . HXT.runLA (HXT.xshowBlob HXT.getChildren
    {- HXT.<<< HXT.addXmlPiEncoding -} HXT.<<< HXT.addXmlPi
    HXT.<<< HXT.processChildren (HXT.cleanupNamespaces HXT.collectPrefixUriPairs))
  . pickleDoc xpickle
