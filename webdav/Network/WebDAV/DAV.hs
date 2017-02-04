-- |DAV element type declarations, based on RFC4918
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ViewPatterns #-}
module Network.WebDAV.DAV
  ( -- * §14 Elements
    ActiveLock(..)
  , Depth(..)
  , Error(..)
  , Location(..)
  , LockEntry(..)
  , LockInfo(..)
  , LockRoot(..)
  , LockScope(..)
  , LockToken(..)
  , LockType(..)
  , MultiStatus(..)
  , Owner(..)
  , PropertyUpdate(..)
  , PropUpdate(..)
  , PropFind(..)
  , PropStat(..)
  , Response(..)
  , ResponseDescription
  , Status
  , Timeout(..)
    -- * §15 Properties
  , PropertyContent
  , Property(..)
  , PropertyType
  , propertyType
  , PropertyValue
  , propertyContent
  ) where

import           Control.Arrow (first)
import           Control.Monad (msum)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import           Data.Char (isDigit)
import           Data.Functor.Classes (Show1, Eq1)
import           Data.Functor.Identity (Identity(..))
import qualified Data.Invertible as Inv
import           Data.Monoid (Alt(..))
import           Data.Proxy (Proxy(..))
import qualified Data.Text as T
import           Data.Text.Encoding (encodeUtf8, decodeUtf8)
import           Data.Time.Clock (UTCTime)
import           Data.Time.Format (formatTime, parseTimeM, defaultTimeLocale)
import           Data.Word (Word32, Word64)
import qualified Data.XML.Types as XT
import           Network.URI (URI)
import qualified Network.HTTP.Types.Status as HTTP
import qualified Text.XML.Stream.Invertible as X
import           Waimwork.HTTP (formatHTTPDate, parseHTTPDate, ETag, renderETag, parseETag)

import           Network.WebDAV.XML

davName :: String -> XMLName
davName n = XT.Name (T.pack n) (Just $ T.pack "DAV:") (Just $ T.pack "D")

davElem :: String -> XMLConverter a -> XMLConverter a
davElem = X.tagNoAttr . davName

-- |§14.1
data ActiveLock = ActiveLock
  { activeLockScope :: LockScope
  , activeLockType :: LockType
  , activeLockDepth :: Depth
  , activeLockOwner :: Maybe Owner
  , activeLockTimeout :: Maybe Timeout
  , activeLockToken :: Maybe LockToken
  , activeLockRoot :: LockRoot
  }
  deriving (Eq, Show)

instance XML ActiveLock where
  xmlConvert = davElem "activelock" $ [X.biCase|
      ((((((s, t), d), o), to), tok), r) <-> ActiveLock s t d o to tok r|]
    X.>$<  (xmlConvert
      X.>*< xmlConvert
      X.>*< xmlConvert
      X.>*< X.optionalI xmlConvert
      X.>*< X.optionalI xmlConvert
      X.>*< X.optionalI xmlConvert
      X.>*< xmlConvert)

-- |§14.4
data Depth
  = Depth0
  | Depth1
  | DepthInfinity
  deriving (Eq, Enum, Bounded)

instance Show Depth where
  showsPrec _ Depth0 = showChar '0'
  showsPrec _ Depth1 = showChar '1'
  showsPrec _ DepthInfinity = showString "infinity"

instance Read Depth where
  readsPrec _ ('0':r) = [(Depth0, r)]
  readsPrec _ ('1':r) = [(Depth1, r)]
  readsPrec _ ('i':'n':'f':'i':'n':'i':'t':'y':r) = [(DepthInfinity, r)]
  readsPrec _ _ = []

instance XML Depth where
  xmlConvert = davElem "depth" X.readShowContent

-- |§14.5
newtype Error = Error XMLTrees
  deriving (Eq, Show)

instance XML Error where
  xmlConvert = davElem "error" $ [X.biCase|x <-> Error x|]
    X.>$< xmlConvert

-- |§14.7
type HRef = URI

xmlHRef :: XMLConverter HRef
xmlHRef = davElem "href" xmlConvert

-- |§14.9
newtype Location = Location{ locationHRef :: HRef }
  deriving (Eq, Show)

instance XML Location where
  xmlConvert = davElem "location" $ [X.biCase|x <-> Location x|]
    X.>$< xmlHRef

-- |§14.10
data LockEntry = LockEntry
  { lockEntryScope :: LockScope
  , lockEntryType :: LockType
  }
  deriving (Eq, Show)

instance XML LockEntry where
  xmlConvert = davElem "lockentry" $ [X.biCase|
      (s, t) <-> LockEntry s t|]
    X.>$<  (xmlConvert
      X.>*< xmlConvert)

-- |§14.11
data LockInfo = LockInfo
  { lockInfoScope :: LockScope
  , lockInfoType :: LockType
  , lockInfoOwner :: Maybe Owner
  }
  deriving (Eq, Show)

instance XML LockInfo where
  xmlConvert = davElem "lockinfo" $ [X.biCase|
      ((s, t), o) <-> LockInfo s t o|]
    X.>$<  (xmlConvert
      X.>*< xmlConvert
      X.>*< X.optionalI xmlConvert)

-- |§14.12
newtype LockRoot = LockRoot{ lockRootHRef :: HRef }
  deriving (Eq, Show)

instance XML LockRoot where
  xmlConvert = davElem "lockroot" $ [X.biCase|x <-> LockRoot x|]
    X.>$< xmlHRef

-- |§14.13
data LockScope
  = LockScopeExclusive
  | LockScopeShared
  deriving (Eq, Enum, Bounded, Show)

instance XML LockScope where
  xmlConvert = davElem "lockscope" $ [X.biCase|
      Left  () <-> LockScopeExclusive
      Right () <-> LockScopeShared |]
    X.>$<  (davElem "exclusive" X.unit
      X.>|< davElem "shared" X.unit)

-- |§14.14
newtype LockToken = LockToken{ lockTokenHRef :: HRef }
  deriving (Eq, Show)

instance XML LockToken where
  xmlConvert = davElem "locktoken" $ [X.biCase|x <-> LockToken x|]
    X.>$< xmlHRef

-- |§14.15
data LockType
  = LockTypeWrite
  deriving (Eq, Enum, Bounded, Show)

instance XML LockType where
  xmlConvert = davElem "locktype" $ [X.biCase|
      () <-> LockTypeWrite |]
    X.>$<   davElem "write" X.unit

-- |§14.16
data MultiStatus = MultiStatus
  { multiStatusResponses :: [Response]
  , multiStatusResponseDescription :: ResponseDescription
  }
  deriving (Eq, Show)

-- |§14.17
newtype Owner = Owner XMLTrees
  deriving (Eq, Show)

instance XML Owner where
  xmlConvert = davElem "owner" $ [X.biCase|x <-> Owner x|]
    X.>$< xmlConvert

-- |§14.18
type Prop c = [Property c]

xmlProp :: PropertyContent c => XMLConverter [Property c]
xmlProp = davElem "prop" xmlConvert

-- |§14.19
newtype PropertyUpdate = PropertyUpdate [PropUpdate]
  deriving (Eq, Show)

instance XML PropertyUpdate where
  xmlConvert = davElem "propertyupdate" $ [X.biCase|
      l <-> PropertyUpdate l|]
    X.>$<  X.manyI xmlConvert -- +

data PropUpdate
  = Remove (Prop WithoutValue)
  | Set (Prop WithValue)
  deriving (Eq, Show)

instance XML PropUpdate where
  xmlConvert = [X.biCase|
      Left  p <-> Remove p
      Right p <-> Set p|]
    X.>$<  (davElem "remove" xmlProp
      X.>|< davElem "set" xmlProp)

-- |§14.20
data PropFind
  = PropName
  | PropFind
    { propFindAll :: !Bool
    , propFind :: Prop WithoutValue
    }
  deriving (Eq, Show)

instance XML PropFind where
  xmlConvert = [X.biCase|
      Left (Left  ())       <-> PropName
      Left (Right Nothing)  <-> PropFind True []
      Left (Right (Just l)) <-> PropFind True l
            Right p         <-> PropFind False p|]
    X.>$<  (davElem "propname" X.unit
      X.>|< davElem "allprop" X.unit X.*< X.optionalI
        (davElem "include" xmlConvert)
      X.>|< xmlProp)

-- |§14.22
data PropStat = PropStat
  { propStatProp :: Prop MaybeValue
  , propStatus :: Status
  , propStatError :: Maybe Error
  , propStatResonseDescription :: Maybe ResponseDescription
  }
  deriving (Eq, Show)

instance XML PropStat where
  xmlConvert = davElem "propstat" $ [X.biCase|
      (((p, s), e), r) <-> PropStat p s e r|]
    X.>$<  (xmlProp
      X.>*< xmlStatus
      X.>*< X.optionalI xmlConvert
      X.>*< X.optionalI xmlResponseDescription)

-- |§14.24
data Response = Response
  { responseHRefs :: [HRef]
  , responseStatus :: Maybe Status
  , responsePropStat :: [PropStat]
  , responseError :: Maybe Error
  , responseDescription :: Maybe ResponseDescription
  , responseLocation :: Maybe Location
  }
  deriving (Eq, Show)

instance XML Response where
  xmlConvert = davElem "response" $ [X.biCase|
      (((((h, s), p), e), d), l) <-> Response h s p e d l|]
    X.>$<  (X.manyI xmlHRef -- +
      X.>*< X.optionalI xmlStatus
      X.>*< X.manyI xmlConvert
      X.>*< X.optionalI xmlConvert
      X.>*< X.optionalI xmlResponseDescription
      X.>*< X.optionalI xmlConvert)

-- |§14.25
type ResponseDescription = T.Text

xmlResponseDescription :: XMLConverter ResponseDescription
xmlResponseDescription = davElem "responsedescription" X.content

-- |§14.28
type Status = HTTP.Status

xmlStatus :: XMLConverter Status
xmlStatus = davElem "status" $ X.convert rd sh X.stringContent where
  rd ('H':'T':'T':'P':'/':(dropWhile isDigit -> '.':(dropWhile isDigit -> ' ':x:y:z:' ':r)))
    | all isDigit s = Right $ HTTP.Status (read s) (BSC.pack r) where s = [x,y,z]
  rd _ = Left "invalid HTTP status line"
  sh (HTTP.Status n m) = "HTTP/1.1 " ++ pad (show n) ++ " " ++ BSC.unpack m where
    pad [x] = ['0','0',x]
    pad [x,y] = ['0',x,y]
    pad z = z

-- |§14.29
newtype Timeout = Timeout{ timeoutSeconds :: Maybe Word32 }
  deriving (Eq)

instance Show Timeout where
  showsPrec _ (Timeout Nothing) = showString "Infinite"
  showsPrec _ (Timeout (Just n)) = showString "Second-" . shows n

instance Read Timeout where
  readsPrec _ ('I':'n':'f':'i':'n':'i':'t':'e':r) = [(Timeout Nothing, r)]
  readsPrec _ ('S':'e':'c':'o':'n':'d':'-':r) = map (first $ Timeout . Just) $ reads r
  readsPrec _ _ = []

instance XML Timeout where
  xmlConvert = davElem "timeout" X.readShowContent


class (Traversable c, Monad c, Show1 c, Eq1 c) => PropertyContent c where
  xmlPropertyContent :: XMLConverter a -> XMLConverter (c a)

type WithoutValue = Proxy
type WithValue = Identity
type MaybeValue = Maybe

instance PropertyContent Proxy where
  xmlPropertyContent _ = X.pureI Proxy
instance PropertyContent Maybe where
  xmlPropertyContent = X.optionalI
instance PropertyContent Identity where
  xmlPropertyContent = Inv.fmap Inv.identity

propertyContent :: PropertyContent c => c a -> Maybe a
propertyContent = getAlt . foldMap (Alt . Just)

-- |Properties, with values guarded by type argument
data Property c
  = CreationDate !(c DateTime) -- ^iso8601
  | DisplayName !(c T.Text)
  | GetContentLanguage !(c BS.ByteString) -- ^language-tag
  | GetContentLength !(c Word64)
  | GetContentType !(c BS.ByteString) -- ^media-type
  | GetETag !(c ETag)
  | GetLastModified !(c UTCTime) -- ^http date
  | LockDiscovery !(c [ActiveLock])
  | ResourceType !(c (Bool, XMLTrees)) -- ^(collection, elements)
  | SupportedLock !(c [LockEntry])
  | Property
    { propertyName :: !XMLName
    , propertyValue :: !(c XMLTrees)
    }

deriving instance {- PropertyContent c => -} Eq   (Property Proxy)
deriving instance {- PropertyContent c => -} Eq   (Property Maybe)
deriving instance {- PropertyContent c => -} Eq   (Property Identity)
deriving instance {- PropertyContent c => -} Ord  (Property Proxy)
deriving instance {- PropertyContent c => -} Show (Property Proxy)
deriving instance {- PropertyContent c => -} Show (Property Maybe)
deriving instance {- PropertyContent c => -} Show (Property Identity)

instance PropertyContent c => XML (Property c) where
  xmlConvert = [X.biCase|
      Left (Left (Left (Left (Left (Left (Left (Left (Left (Left  c))))))))) <-> CreationDate c
      Left (Left (Left (Left (Left (Left (Left (Left (Left (Right c))))))))) <-> DisplayName c
            Left (Left (Left (Left (Left (Left (Left (Left (Right c))))))))  <-> GetContentLanguage c
                  Left (Left (Left (Left (Left (Left (Left (Right c)))))))   <-> GetContentLength c
                        Left (Left (Left (Left (Left (Left (Right c))))))    <-> GetContentType c
                              Left (Left (Left (Left (Left (Right c)))))     <-> GetETag c
                                    Left (Left (Left (Left (Right c))))      <-> GetLastModified c
                                          Left (Left (Left (Right c)))       <-> LockDiscovery c
                                                Left (Left (Right c))        <-> ResourceType c
                                                      Left (Right c)         <-> SupportedLock c
                                                            Right (p, (), c) <-> Property p c|]
    X.>$<  (davElem "creationdate"       (xmlPropertyContent xmlDateTime)
      X.>|< davElem "displayname"        (xmlPropertyContent X.content)
      X.>|< davElem "getcontentlanguage" (xmlPropertyContent xmlHTTPHeader)
      X.>|< davElem "getcontentlength"   (xmlPropertyContent X.readShowContent)
      X.>|< davElem "getcontenttype"     (xmlPropertyContent xmlHTTPHeader)
      X.>|< davElem "getetag"            (xmlPropertyContent xmlETag)
      X.>|< davElem "getlastmodified"    (xmlPropertyContent xmlHTTPDate)
      X.>|< davElem "lockdiscovery"      (xmlPropertyContent $ X.manyI xmlConvert)
      X.>|< davElem "resourcetype"       (xmlPropertyContent $
        Inv.isJust X.>$< X.optionalI (davElem "collection" X.unit)
          X.>*< X.passNodes)
      X.>|< davElem "supportedlock"      (xmlPropertyContent $ X.manyI xmlConvert)
      X.>|< X.tag X.unit                 (xmlPropertyContent X.passNodes))

type PropertyType = Property WithoutValue
type PropertyValue = Property WithValue

propertyType :: (WithoutValue a -> PropertyType) -> PropertyType
propertyType c = c Proxy

type DateTime = UTCTime

xmlDateTime :: XMLConverter DateTime
xmlDateTime = X.convert rd sh X.stringContent where
  rd s = maybe (Left "invalid iso8601 date") Right $ msum $ map (\f -> parseTimeM True defaultTimeLocale f s) fmt
  sh = formatTime defaultTimeLocale $ head fmt
  fmt = ["%FT%T%QZ", "%FT%T%Q%z"]

xmlHTTPHeader :: XMLConverter BS.ByteString
xmlHTTPHeader = (encodeUtf8 . T.strip X.:<->: decodeUtf8) X.>$< X.content

xmlHTTPDate :: XMLConverter UTCTime
xmlHTTPDate = X.convert (maybe (Left "invalid HTTP date") Right . parseHTTPDate) formatHTTPDate xmlHTTPHeader

xmlETag :: XMLConverter ETag
xmlETag = X.convert parseETag renderETag xmlHTTPHeader
