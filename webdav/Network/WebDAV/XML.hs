{-# OPTIONS_GHC -fno-warn-orphans #-}
module Network.WebDAV.XML
  ( module Text.XML.HXT.Arrow.Pickle.Xml
  , Inv.biCase
  , module Control.Invertible.Monoidal
  , xpWhitespace
  , xpAny
  , xpTrimAnyElem
  , xpTrimAnyElems
  , xpTrimElemNS
  , xpTrimNameElem
  , xpURI
  , toXMLDoc
  ) where

import           Control.Invertible.Monoidal
import           Control.Monad.State.Class (gets, modify, state)
import qualified Data.ByteString.Lazy as BSL
import           Data.Char.Properties.XMLCharProps (isXmlSpaceChar)
import qualified Data.Invertible as Inv
import qualified Network.URI as URI
import           Text.XML.HXT.Arrow.Pickle.Schema (Schema(Any), scEmpty, scSeq, scAlt, scDTxsd)
import           Text.XML.HXT.Arrow.Pickle.Xml
import qualified Text.XML.HXT.Core as HXT
import qualified Text.XML.HXT.DOM.XmlNode as XN
import qualified Text.XML.HXT.XMLSchema.DataTypeLibW3CNames as XSD

instance Inv.Functor PU where
  fmap (f Inv.:<->: g) p = PU -- xpWrap
    { appPickle = appPickle p . g
    , appUnPickle = fmap f $ appUnPickle p
    , theSchema = theSchema p
    }
instance Monoidal PU where
  unit = xpUnit
  p >*< q = PU -- xpPair
    { appPickle = \(a, b) -> appPickle p a . appPickle q b
    , appUnPickle = do
        a <- appUnPickle p 
        b <- appUnPickle q
        return (a, b)
    , theSchema = theSchema p `scSeq` theSchema q
    }
instance MonoidalAlt PU where
  p >|< q = PU
    { appPickle = either (appPickle p) (appPickle q)
    , appUnPickle = mchoice (Left <$> appUnPickle p) return (Right <$> appUnPickle q)
    , theSchema = theSchema p `scAlt` theSchema q
    }

-- |Ignore any whitespace and produce nothing
xpWhitespace :: PU ()
xpWhitespace = PU
  { appPickle = const id
  , appUnPickle = modify $ \s -> s{ contents = dropWhile (any (all isXmlSpaceChar) . XN.getText) $ contents s }
  , theSchema = scEmpty
  }

xpAny :: PU HXT.XmlTrees
xpAny = PU
  { appPickle = \c s -> s{ contents = c ++ contents s }
  , appUnPickle = state $ \s -> (contents s, s{ contents = [] })
  , theSchema = Any -- XXX
  }

xpTrimAnyElem :: PU HXT.XmlTree
xpTrimAnyElem = xpWhitespace *< xpWrapEither 
  ( \e -> if XN.isElem e then Right e else Left "xpAnyElem: any element expected"
  , id
  ) xpTree

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
    HXT.<<< HXT.addXmlPiEncoding HXT.<<< HXT.addXmlPi
    HXT.<<< HXT.processChildren (HXT.cleanupNamespaces HXT.collectPrefixUriPairs))
  . pickleDoc xpickle
