{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Network.NFS.V4.String
  ( NFSStrCS(..)
  , NFSStrCIS(..)
  , NFSStrMixed(..)
  ) where

import qualified Data.ByteString.Char8 as BSC
import qualified Data.CaseInsensitive as CI
import           Data.Monoid ((<>))
import           Data.String (IsString(..))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Network.ONCRPC.XDR.Opaque (Opaqued(..))

-- |For 'NFS.Utf8str_cs'.
newtype NFSStrCS = NFSStrCS{ nfsStrCSText :: T.Text }
  deriving (Eq, Ord, IsString)
-- |For 'NFS.Utf8str_cis'.
newtype NFSStrCIS = NFSStrCIS{ nfsStrCISText :: CI.CI T.Text }
  deriving (Eq, Ord, IsString)
-- |For 'NFS.Utf8str_mixed', components are separated with \"\@\".
data NFSStrMixed = NFSStrMixed
  { nfsStrMixedPrefix :: NFSStrCS
  , nfsStrMixedDomain :: Maybe NFSStrCIS
  }
  deriving (Eq, Ord)

instance IsString NFSStrMixed where
  fromString s = case break ('@' ==) s of
    (p, []) -> NFSStrMixed (fromString p) Nothing
    (p, ~('@':d)) -> NFSStrMixed (fromString p) (Just $ fromString d)

instance Opaqued NFSStrCS where
  opacify = TE.encodeUtf8 . nfsStrCSText
  unopacify = either (fail . show) (return . NFSStrCS) . TE.decodeUtf8'

instance Opaqued NFSStrCIS where
  opacify = TE.encodeUtf8 . CI.original . nfsStrCISText
  unopacify = either (fail . show) (return . NFSStrCIS . CI.mk) . TE.decodeUtf8'

instance Opaqued NFSStrMixed where
  opacify (NFSStrMixed p d) = opacify p <> foldMap (BSC.cons '@' . opacify) d
  unopacify s = maybe
    ((`NFSStrMixed` Nothing) <$> unopacify s)
    (\i -> NFSStrMixed <$> unopacify (BSC.take i s) <*> (Just <$> unopacify (BSC.drop (succ i) s)))
    $ BSC.elemIndexEnd '@' s
    
