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
  , Include(..)
  , Location(..)
  , LockEntry(..)
  , LockInfo(..)
  , LockRoot(..)
  , LockScope(..)
  , LockToken(..)
  , LockType(..)
  , MultiStatus(..)
  , Owner(..)
  , Prop(..)
  , PropertyUpdate(..)
  , PropUpdate(..)
  , PropFind(..)
  , PropStat(..)
  , Response(..)
  , ResponseDescription
  , Status
  , Timeout(..)
    -- * §15 Properties
  , Property(..)
  , propertyContent
  ) where

import           Control.Arrow (first)
import           Control.Monad (msum)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import           Data.Char (isDigit, isSpace)
import           Data.Functor.Classes (Show1, Eq1)
import           Data.Functor.Identity (Identity(..))
import qualified Data.Invertible as Inv
import           Data.Monoid (Alt(..))
import           Data.Proxy (Proxy(..))
import           Data.Time.Clock (UTCTime)
import           Data.Time.Format (formatTime, parseTimeM, defaultTimeLocale)
import           Data.Word (Word32, Word64)
import           Network.URI (URI)
import qualified Network.HTTP.Types.Status as HTTP
import qualified Text.XML.HXT.DOM.TypeDefs as XT
import           Waimwork.HTTP (formatHTTPDate, parseHTTPDate, ETag, renderETag, parseETag)

import qualified Network.WebDAV.XML as X

xpElem :: String -> X.PU a -> X.PU a
xpElem n c = X.xpTrimElemNS "DAV:" "D" n c

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

instance X.XmlPickler ActiveLock where
  xpickle = xpElem "activelock" $ [X.biCase|
      ((((((s, t), d), o), to), tok), r) <-> ActiveLock s t d o to tok r|]
    X.>$<  (X.xpickle
      X.>*< X.xpickle
      X.>*< X.xpickle
      X.>*< X.xpOption X.xpickle
      X.>*< X.xpOption X.xpickle
      X.>*< X.xpOption X.xpickle
      X.>*< X.xpickle)

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

instance X.XmlPickler Depth where
  xpickle = xpElem "depth" X.xpPrim

-- |§14.5
newtype Error = Error XT.XmlTrees
  deriving (Eq, Show)

instance X.XmlPickler Error where
  xpickle = xpElem "error" $ [X.biCase|x <-> Error x|]
    X.>$< X.xpAny

-- |§14.7
type HRef = URI

xpHRef :: X.PU URI
xpHRef = xpElem "href" X.xpURI

-- |§14.8
newtype Include = Include XT.XmlTrees -- elements only
  deriving (Eq, Show)

instance X.XmlPickler Include where
  xpickle = xpElem "include" $ [X.biCase|x <-> Include x|]
    X.>$< X.xpTrimAnyElems

-- |§14.9
newtype Location = Location{ locationHRef :: HRef }
  deriving (Eq, Show)

instance X.XmlPickler Location where
  xpickle = xpElem "location" $ [X.biCase|x <-> Location x|]
    X.>$< xpHRef

-- |§14.10
data LockEntry = LockEntry
  { lockEntryScope :: LockScope
  , lockEntryType :: LockType
  }
  deriving (Eq, Show)

instance X.XmlPickler LockEntry where
  xpickle = xpElem "lockentry" $ [X.biCase|
      (s, t) <-> LockEntry s t|]
    X.>$<  (X.xpickle
      X.>*< X.xpickle)

-- |§14.11
data LockInfo = LockInfo
  { lockInfoScope :: LockScope
  , lockInfoType :: LockType
  , lockInfoOwner :: Maybe Owner
  }
  deriving (Eq, Show)

instance X.XmlPickler LockInfo where
  xpickle = xpElem "lockinfo" $ [X.biCase|
      ((s, t), o) <-> LockInfo s t o|]
    X.>$<  (X.xpickle
      X.>*< X.xpickle
      X.>*< X.xpOption X.xpickle)

-- |§14.12
newtype LockRoot = LockRoot{ lockRootHRef :: HRef }
  deriving (Eq, Show)

instance X.XmlPickler LockRoot where
  xpickle = xpElem "lockroot" $ [X.biCase|x <-> LockRoot x|]
    X.>$< xpHRef

-- |§14.13
data LockScope
  = LockScopeExclusive
  | LockScopeShared
  deriving (Eq, Enum, Bounded, Show)

instance X.XmlPickler LockScope where
  xpickle = xpElem "lockscope" $ [X.biCase|
      Left  () <-> LockScopeExclusive
      Right () <-> LockScopeShared |]
    X.>$<  (xpElem "exclusive" X.xpUnit
      X.>|< xpElem "shared" X.xpUnit)

-- |§14.14
newtype LockToken = LockToken{ lockTokenHRef :: HRef }
  deriving (Eq, Show)

instance X.XmlPickler LockToken where
  xpickle = xpElem "locktoken" $ [X.biCase|x <-> LockToken x|]
    X.>$< xpHRef

-- |§14.15
data LockType
  = LockTypeWrite
  deriving (Eq, Enum, Bounded, Show)

instance X.XmlPickler LockType where
  xpickle = xpElem "locktype" $ [X.biCase|
      () <-> LockTypeWrite |]
    X.>$<   xpElem "write" X.xpUnit

-- |§14.16
data MultiStatus = MultiStatus
  { multiStatusResponses :: [Response]
  , multiStatusResponseDescription :: ResponseDescription
  }
  deriving (Eq, Show)

-- |§14.17
newtype Owner = Owner XT.XmlTrees
  deriving (Eq, Show)

instance X.XmlPickler Owner where
  xpickle = xpElem "owner" $ [X.biCase|x <-> Owner x|]
    X.>$< X.xpAny

-- |§14.18
newtype Prop c = Prop [Property c]

deriving instance {- PropertyContent c => -} Eq   (Prop Proxy)
deriving instance {- PropertyContent c => -} Eq   (Prop Maybe)
deriving instance {- PropertyContent c => -} Eq   (Prop Identity)
deriving instance {- PropertyContent c => -} Show (Prop Proxy)
deriving instance {- PropertyContent c => -} Show (Prop Maybe)
deriving instance {- PropertyContent c => -} Show (Prop Identity)

instance PropertyContent c => X.XmlPickler (Prop c) where
  xpickle = xpElem "prop" $ [X.biCase|x <-> Prop x|]
    X.>$< X.xpList X.xpickle

-- |§14.19
newtype PropertyUpdate = PropertyUpdate [PropUpdate]
  deriving (Eq, Show)

instance X.XmlPickler PropertyUpdate where
  xpickle = xpElem "propertyupdate" $ [X.biCase|
      l <-> PropertyUpdate l|]
    X.>$<  X.xpList1 X.xpickle

data PropUpdate
  = Remove (Prop WithoutValue)
  | Set (Prop WithValue)
  deriving (Eq, Show)

instance X.XmlPickler PropUpdate where
  xpickle = [X.biCase|
      Left  p <-> Remove p
      Right p <-> Set p|]
    X.>$<  (xpElem "remove" X.xpickle
      X.>|< xpElem "set" X.xpickle)

-- |§14.20
data PropFind
  = PropName
  | PropAll{ propFindInclude :: Maybe Include }
  | PropFind (Prop WithoutValue)
  deriving (Eq, Show)

instance X.XmlPickler PropFind where
  xpickle = [X.biCase|
      Left (Left  ()) <-> PropName
      Left (Right i)  <-> PropAll i
            Right p   <-> PropFind p|]
    X.>$<  (xpElem "propname" X.xpUnit
      X.>|< xpElem "allprop" X.xpUnit X.*< X.xpOption X.xpickle
      X.>|< X.xpickle)

-- |§14.22
data PropStat = PropStat
  { propStatProp :: Prop MaybeValue
  , propStatus :: Status
  , propStatError :: Maybe Error
  , propStatResonseDescription :: Maybe ResponseDescription
  }
  deriving (Eq, Show)

instance X.XmlPickler PropStat where
  xpickle = xpElem "propstat" $ [X.biCase|
      (((p, s), e), r) <-> PropStat p s e r|]
    X.>$<  (X.xpickle
      X.>*< xpStatus
      X.>*< X.xpOption X.xpickle
      X.>*< X.xpOption xpResponseDescription)

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

instance X.XmlPickler Response where
  xpickle = xpElem "response" $ [X.biCase|
      (((((h, s), p), e), d), l) <-> Response h s p e d l|]
    X.>$<  (X.xpList1 xpHRef
      X.>*< X.xpOption xpStatus
      X.>*< X.xpList X.xpickle
      X.>*< X.xpOption X.xpickle
      X.>*< X.xpOption xpResponseDescription
      X.>*< X.xpOption X.xpickle)

-- |§14.25
type ResponseDescription = String

xpResponseDescription :: X.PU ResponseDescription
xpResponseDescription = xpElem "responsedescription" X.xpText0

-- |§14.28
type Status = HTTP.Status

xpStatus :: X.PU Status
xpStatus = xpElem "status" $ X.xpWrapEither (rd, sh) X.xpText where
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

instance X.XmlPickler Timeout where
  xpickle = xpElem "timeout" X.xpPrim


class (Traversable c, Monad c, Show1 c, Eq1 c) => PropertyContent c where
  xpPropertyContent :: X.PU a -> X.PU (c a)

type WithoutValue = Proxy
type WithValue = Identity
type MaybeValue = Maybe

instance PropertyContent Proxy where
  xpPropertyContent _ = X.pureI Proxy
instance PropertyContent Maybe where
  xpPropertyContent = X.xpOption
instance PropertyContent Identity where
  xpPropertyContent = Inv.fmap Inv.identity

propertyContent :: PropertyContent c => c a -> Maybe a
propertyContent = getAlt . foldMap (Alt . Just)

-- |Properties, with values guarded by type argument
data Property c
  = CreationDate (c DateTime) -- ^iso8601
  | DisplayName (c String)
  | GetContentLanguage (c BS.ByteString) -- ^language-tag
  | GetContentLength (c Word64)
  | GetContentType (c BS.ByteString) -- ^media-type
  | GetETag (c ETag)
  | GetLastModified (c UTCTime) -- ^http date
  | LockDiscovery (c [ActiveLock])
  | ResourceType (c (Bool, XT.XmlTrees)) -- ^(collection, elements)
  | SupportedLock (c [LockEntry])
  | Property
    { propertyName :: XT.QName
    , propertyValue :: c XT.XmlTrees
    }

deriving instance {- PropertyContent c => -} Eq   (Property Proxy)
deriving instance {- PropertyContent c => -} Eq   (Property Maybe)
deriving instance {- PropertyContent c => -} Eq   (Property Identity)
deriving instance {- PropertyContent c => -} Show (Property Proxy)
deriving instance {- PropertyContent c => -} Show (Property Maybe)
deriving instance {- PropertyContent c => -} Show (Property Identity)

instance PropertyContent c => X.XmlPickler (Property c) where
  xpickle = [X.biCase|
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
                                                            Right (p, c)     <-> Property p c|]
    X.>$<  (xpElem "creationdate"       (xpPropertyContent xpDateTime)
      X.>|< xpElem "displayname"        (xpPropertyContent X.xpText0)
      X.>|< xpElem "getcontentlanguage" (xpPropertyContent xpHTTPHeader)
      X.>|< xpElem "getcontentlength"   (xpPropertyContent X.xpPrim)
      X.>|< xpElem "getcontenttype"     (xpPropertyContent xpHTTPHeader)
      X.>|< xpElem "getetag"            (xpPropertyContent xpETag)
      X.>|< xpElem "getlastmodified"    (xpPropertyContent xpHTTPDate)
      X.>|< xpElem "lockdiscovery"      (xpPropertyContent $ X.xpList X.xpickle)
      X.>|< xpElem "resourcetype"       (xpPropertyContent $
        Inv.isJust X.>$< X.xpOption (xpElem "collection" X.xpUnit)
          X.>*< X.xpTrimAnyElems)
      X.>|< xpElem "supportedlock"      (xpPropertyContent $ X.xpList X.xpickle)
      X.>|< X.xpTrimNameElem            (xpPropertyContent X.xpAny))

type DateTime = UTCTime

xpDateTime :: X.PU DateTime
xpDateTime = X.xpWrapEither (rd, sh) X.xpText where
  rd s = maybe (Left "invalid iso8601 date") Right $ msum $ map (\f -> parseTimeM True defaultTimeLocale f s) fmt
  sh = formatTime defaultTimeLocale $ head fmt
  fmt = ["%FT%T%QZ", "%FT%T%Q%z"]

xpHTTPHeader :: X.PU BS.ByteString
xpHTTPHeader = X.xpWrap (BSC.pack . dropWhile isSpace, BSC.unpack) X.xpText

xpHTTPDate :: X.PU UTCTime
xpHTTPDate = X.xpWrapEither (maybe (Left "invalid HTTP date") Right . parseHTTPDate, formatHTTPDate) xpHTTPHeader

xpETag :: X.PU ETag
xpETag = X.xpWrap (parseETag, renderETag) xpHTTPHeader
