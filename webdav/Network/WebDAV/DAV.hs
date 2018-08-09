-- |DAV element type declarations, based on RFC4918
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ViewPatterns #-}
module Network.WebDAV.DAV
  ( -- * §11 Status codes
    multiStatus207
  , HTTP.unprocessableEntity422
  , locked423
  , failedDependency424
  , insufficientStorage507
    -- * §14 Elements
  , ActiveLock(..)
  , Depth(..)
  , predDepth
  , Error
  , HRef
  , Location(..)
  , LockEntry(..)
  , LockInfo(..)
  , LockRoot(..)
  , LockScope(..)
  , LockToken(..)
  , LockType(..)
  , MultiStatus(..)
  , streamMultiStatus
  , Owner(..)
  , Prop
  , PropertyUpdate(..)
  , PropUpdate(..)
  , PropFind(..)
  , PropStat(..)
  , Response(..)
  , responseHRefs
  , ResponseDescription
  , Status
  , Timeout(..)
    -- * §15 Properties
  , PropertyContent
  , Property(..)
  , PropertyType
  , Proxy(Proxy)
  , PropertyValue
  , mapProperty
  , propertyContent
    -- * §16 Errors
  , ErrorElement(..)
  , errorElementStatus
  , errorElementIsPostcondition
  , ErrorElement'
  ) where

import           Control.Arrow (first)
import           Control.Monad (msum)
import           Control.Monad.Catch (MonadThrow)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import           Data.Char (isDigit)
import qualified Data.Conduit as C
import           Data.Functor.Classes (Show1(..), Eq1(..))
import           Data.Functor.Identity (Identity(..))
import qualified Data.Invertible as Inv
import           Data.Monoid (Alt(..))
import           Data.Proxy (Proxy(..))
import qualified Data.Text as T
import           Data.Text.Encoding (encodeUtf8, decodeUtf8)
import           Data.Time.Clock (UTCTime)
import           Data.Time.Format (formatTime, parseTimeM, defaultTimeLocale)
import           Data.Semigroup (Semigroup(..))
import           Data.Word (Word32, Word64)
import qualified Data.XML.Types as XT
import           Network.URI (URI)
import qualified Network.HTTP.Types.Status as HTTP
import qualified Text.XML.Stream.Invertible as X
import qualified Text.XML.Stream.Render as XR
import           Waimwork.HTTP (formatHTTPDate, parseHTTPDate, ETag, renderETag, parseETag)

import           Network.WebDAV.XML

-- |§11.1
multiStatus207 :: HTTP.Status
multiStatus207 = HTTP.mkStatus 207 "Multi-Status"

-- |§11.2
-- unprocessableEntity422 = HTTP.unprocessableEntity422

-- |§11.3
locked423 :: HTTP.Status
locked423 = HTTP.mkStatus 423 "Locked"

-- |§11.4
failedDependency424 :: HTTP.Status
failedDependency424 = HTTP.mkStatus 424 "Failed Dependency"

-- |§11.5
insufficientStorage507 :: HTTP.Status
insufficientStorage507 = HTTP.mkStatus 507 "Insufficient Storage"

davName :: String -> XMLName
davName n = XT.Name (T.pack n) (Just $ T.pack "DAV:") (Just $ T.pack "D")

davElem :: MonadThrow m => String -> XMLConverter m a -> XMLConverter m a
davElem = X.tagIgnoreAttrs . davName

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
  deriving (Eq, Ord, Enum, Bounded)

instance Show Depth where
  showsPrec _ Depth0 = showChar '0'
  showsPrec _ Depth1 = showChar '1'
  showsPrec _ DepthInfinity = showString "infinity"

instance Read Depth where
  readsPrec _ ('0':r) = [(Depth0, r)]
  readsPrec _ ('1':r) = [(Depth1, r)]
  readsPrec _ ('i':'n':'f':'i':'n':'i':'t':'y':r) = [(DepthInfinity, r)]
  readsPrec _ _ = []

instance Semigroup Depth where
  (<>) = max

instance Monoid Depth where
  mempty = Depth0
  mappend = max

instance XML Depth where
  xmlConvert = davElem "depth" X.readShowContent

predDepth :: Depth -> Depth
predDepth DepthInfinity = DepthInfinity
predDepth Depth1 = Depth0
predDepth Depth0 = error "predDepth Depth0"

-- |§14.5
type Error = [ErrorElement']

xmlError :: MonadThrow m => XMLConverter m Error
xmlError = davElem "error" xmlConvert

-- |§14.7
type HRef = URI

xmlHRef :: MonadThrow m => XMLConverter m HRef
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
  , multiStatusResponseDescription :: Maybe ResponseDescription
  }
  deriving (Eq, Show)

instance XML MultiStatus where
  xmlConvert = davElem "multistatus" $ [X.biCase|
      (l, d) <-> MultiStatus l d|]
    X.>$<  (xmlConvert
      X.>*< X.optionalI xmlResponseDescription)

streamMultiStatus :: MonadThrow m => C.Source m Response -> Maybe ResponseDescription -> C.Source m XT.Event
streamMultiStatus rs d = XR.tag (davName "multistatus") mempty $ do
  rs C..| C.awaitForever (C.toProducer . X.streamerRender xmlConvert)
  mapM_ (X.streamerRender xmlResponseDescription) d

-- |§14.17
newtype Owner = Owner XMLTrees
  deriving (Eq, Show)

instance XML Owner where
  xmlConvert = davElem "owner" $ [X.biCase|x <-> Owner x|]
    X.>$< xmlConvert

-- |§14.18
type Prop c = [Property c]

xmlProp :: (MonadThrow m, PropertyContent c) => XMLConverter m [Property c]
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
  xmlConvert = davElem "propfind" $ [X.biCase|
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
  , propStatError :: Error
  , propStatResonseDescription :: Maybe ResponseDescription
  }
  deriving (Eq, Show)

instance XML PropStat where
  xmlConvert = davElem "propstat" $ [X.biCase|
      (((p, s), Nothing), r) <-> PropStat p s [] r
      (((p, s), (Just e)), r) <-> PropStat p s e r|]
    X.>$<  (xmlProp
      X.>*< xmlStatus
      X.>*< X.optionalI xmlError
      X.>*< X.optionalI xmlResponseDescription)

-- |§14.24
data Response
  = Response
    { responseHRef :: HRef
    , responseHRef_ :: [HRef]
    , responseStatus :: Status
    , responseError :: Error
    , responseDescription :: Maybe ResponseDescription
    , responseLocation :: Maybe Location
    }
  | ResponseProp
    { responseHRef :: HRef
    , responsePropStat :: [PropStat]
    , responseError :: Error
    , responseDescription :: Maybe ResponseDescription
    , responseLocation :: Maybe Location
    }
  deriving (Eq, Show)

responseHRefs :: Response -> [HRef]
responseHRefs Response{ responseHRef = h, responseHRef_ = l } = h:l
responseHRefs ResponseProp{ responseHRef = h } = [h]

instance XML Response where
  xmlConvert = davElem "response" $ [X.biCase|
      ((((h, Left (r, s)), Nothing), d), l) <-> Response   h r s [] d l
      ((((h, Left (r, s)), Just e ), d), l) <-> Response   h r s e d l
      ((((h, Right p    ), Nothing), d), l) <-> ResponseProp h p [] d l
      ((((h, Right p    ), Just e ), d), l) <-> ResponseProp h p e d l|]
    X.>$<  (xmlHRef
      X.>*< (X.manyI xmlHRef X.>*< xmlStatus X.>|< X.manyI xmlConvert)
      X.>*< X.optionalI xmlError
      X.>*< X.optionalI xmlResponseDescription
      X.>*< X.optionalI xmlConvert)

-- |§14.25
type ResponseDescription = T.Text

xmlResponseDescription :: MonadThrow m => XMLConverter m ResponseDescription
xmlResponseDescription = davElem "responsedescription" X.content

-- |§14.28
type Status = HTTP.Status

xmlStatus :: MonadThrow m => XMLConverter m Status
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
  xmlPropertyContent :: XMLConverter m a -> XMLConverter m (c a)

type WithoutValue = Proxy
type WithValue = Identity
type MaybeValue = Maybe

#if !MIN_VERSION_base(4,9,0)
instance Eq1 Proxy where
  eq1 _ _ = True
instance Show1 Proxy where
  showsPrec1 _ _ = showString "Proxy"
#endif

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

mapProperty :: (forall a . c a -> d a) -> Property c -> Property d
mapProperty f (CreationDate       x) = CreationDate       $ f x
mapProperty f (DisplayName        x) = DisplayName        $ f x
mapProperty f (GetContentLanguage x) = GetContentLanguage $ f x
mapProperty f (GetContentLength   x) = GetContentLength   $ f x
mapProperty f (GetContentType     x) = GetContentType     $ f x
mapProperty f (GetETag            x) = GetETag            $ f x
mapProperty f (GetLastModified    x) = GetLastModified    $ f x
mapProperty f (LockDiscovery      x) = LockDiscovery      $ f x
mapProperty f (ResourceType       x) = ResourceType       $ f x
mapProperty f (SupportedLock      x) = SupportedLock      $ f x
mapProperty f (Property n         x) = Property n         $ f x

type DateTime = UTCTime

xmlDateTime :: MonadThrow m => XMLConverter m DateTime
xmlDateTime = X.convert rd sh X.stringContent where
  rd s = maybe (Left "invalid iso8601 date") Right $ msum $ map (\f -> parseTimeM True defaultTimeLocale f s) fmt
  sh = formatTime defaultTimeLocale $ head fmt
  fmt = ["%FT%T%QZ", "%FT%T%Q%z"]

xmlHTTPHeader :: MonadThrow m => XMLConverter m BS.ByteString
xmlHTTPHeader = (encodeUtf8 . T.strip X.:<->: decodeUtf8) X.>$< X.content

xmlHTTPDate :: MonadThrow m => XMLConverter m UTCTime
xmlHTTPDate = X.convert (maybe (Left "invalid HTTP date") Right . parseHTTPDate) formatHTTPDate xmlHTTPHeader

xmlETag :: MonadThrow m => XMLConverter m ETag
xmlETag = X.convert parseETag renderETag xmlHTTPHeader

data ErrorElement
  = LockTokenMatchesRequestURI
  | LockTokenSubmitted [HRef]
  | NoConflictingLock [HRef]
  | NoExternalEntities
  | PreservedLiveProperties
  | PropfindFiniteDepth
  | CannotModifyProtectedProperty
  deriving (Eq, Show)

instance XML ErrorElement where
  xmlConvert = [X.biCase|
      Left (Left (Left (Left (Left (Left ()))))) <-> LockTokenMatchesRequestURI
      Left (Left (Left (Left (Left (Right l))))) <-> LockTokenSubmitted l
            Left (Left (Left (Left (Right l))))  <-> NoConflictingLock l
                  Left (Left (Left (Right ())))  <-> NoExternalEntities
                        Left (Left (Right ()))   <-> PreservedLiveProperties
                              Left (Right ())    <-> PropfindFiniteDepth
                                    Right ()     <-> CannotModifyProtectedProperty|]
    X.>$<  (davElem "lock-token-matches-request-uri" X.unit
      X.>|< davElem "lock-token-submitted" (X.manyI xmlHRef)
      X.>|< davElem "no-conflicting-lock" (X.manyI xmlHRef)
      X.>|< davElem "no-external-entities" X.unit
      X.>|< davElem "preserved-live-properties" X.unit
      X.>|< davElem "propfind-finite-depth" X.unit
      X.>|< davElem "cannot-modify-protected-property" X.unit)

errorElementStatus :: ErrorElement -> HTTP.Status
errorElementStatus LockTokenMatchesRequestURI{}    = HTTP.conflict409
errorElementStatus LockTokenSubmitted{}            = locked423
errorElementStatus NoConflictingLock{}             = locked423
errorElementStatus NoExternalEntities{}            = HTTP.forbidden403
errorElementStatus PreservedLiveProperties{}       = HTTP.conflict409
errorElementStatus PropfindFiniteDepth{}           = HTTP.forbidden403
errorElementStatus CannotModifyProtectedProperty{} = HTTP.forbidden403

errorElementIsPostcondition :: ErrorElement -> Bool
errorElementIsPostcondition PreservedLiveProperties{} = True
errorElementIsPostcondition _ = False

type ErrorElement' = Either XT.Node ErrorElement

instance XML ErrorElement' where
  xmlConvert = Inv.switch X.>$< (xmlConvert X.>|< xmlConvert)
