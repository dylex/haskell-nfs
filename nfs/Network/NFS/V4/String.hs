{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Network.NFS.V4.String
  ( StrCS(..)
  , StrCIS(..)
  , StrMixed(..)
  ) where

import qualified Data.ByteString.Char8 as BSC
import qualified Data.CaseInsensitive as CI
import           Data.Monoid ((<>))
import           Data.String (IsString(..))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Network.ONCRPC.XDR.Opaque (Opaqued(..))

-- |For 'NFS.Utf8str_cs'.
newtype StrCS = StrCS{ strCSText :: T.Text }
  deriving (Eq, Ord, IsString)
-- |For 'NFS.Utf8str_cis'.
newtype StrCIS = StrCIS{ strCISText :: CI.CI T.Text }
  deriving (Eq, Ord, IsString)
-- |For 'NFS.Utf8str_mixed', components are separated with \"\@\".
data StrMixed = StrMixed
  { strMixedPrefix :: StrCS
  , strMixedDomain :: Maybe StrCIS
  }
  deriving (Eq, Ord)

instance IsString StrMixed where
  fromString s = case break ('@' ==) s of
    (p, []) -> StrMixed (fromString p) Nothing
    (p, ~('@':d)) -> StrMixed (fromString p) (Just $ fromString d)

instance Opaqued StrCS where
  opacify = TE.encodeUtf8 . strCSText
  unopacify = either (fail . show) (return . StrCS) . TE.decodeUtf8'

instance Opaqued StrCIS where
  opacify = TE.encodeUtf8 . CI.original . strCISText
  unopacify = either (fail . show) (return . StrCIS . CI.mk) . TE.decodeUtf8'

instance Opaqued StrMixed where
  opacify (StrMixed p d) = opacify p <> foldMap (BSC.cons '@' . opacify) d
  unopacify s = maybe
    ((`StrMixed` Nothing) <$> unopacify s)
    (\i -> StrMixed <$> unopacify (BSC.take i s) <*> (Just <$> unopacify (BSC.drop (succ i) s)))
    $ BSC.elemIndexEnd '@' s
    
