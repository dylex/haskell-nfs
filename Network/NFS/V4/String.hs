{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Network.NFS.V4.String
  ( NFS4CS(..)
  , NFS4CIS(..)
  , NFS4Mixed(..)
  ) where

import qualified Data.ByteString.Char8 as BSC
import qualified Data.CaseInsensitive as CI
import           Data.Monoid ((<>))
import           Data.String (IsString(..))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Network.ONCRPC.XDR.Opaque (Opaqued(..))

import qualified Network.NFS.V4.Prot as NFS

-- |For 'NFS.Utf8str_cs'.
newtype NFS4CS = NFS4CS{ nfs4csText :: T.Text }
  deriving (Eq, Ord, IsString)
-- |For 'NFS.Utf8str_cis'.
newtype NFS4CIS = NFS4CIS{ nfs4cisText :: CI.CI T.Text }
  deriving (Eq, Ord, IsString)
-- |For 'NFS.Utf8str_mixed', components are separated with \"\@\".
data NFS4Mixed = NFS4Mixed
  { nfs4MixedPrefix :: NFS4CS
  , nfs4MixedDomain :: Maybe NFS4CIS
  }
  deriving (Eq, Ord)

instance IsString NFS4Mixed where
  fromString s = case break ('@' ==) s of
    (p, []) -> NFS4Mixed (fromString p) Nothing
    (p, ~('@':d)) -> NFS4Mixed (fromString p) (Just $ fromString d)

instance Opaqued NFS4CS where
  opacify = TE.encodeUtf8 . nfs4csText
  unopacify = either (fail . show) (return . NFS4CS) . TE.decodeUtf8'

instance Opaqued NFS4CIS where
  opacify = TE.encodeUtf8 . CI.original . nfs4cisText
  unopacify = either (fail . show) (return . NFS4CIS . CI.mk) . TE.decodeUtf8'

instance Opaqued NFS4Mixed where
  opacify (NFS4Mixed p d) = opacify p <> foldMap (BSC.cons '@' . opacify) d
  unopacify s = maybe
    ((`NFS4Mixed` Nothing) <$> unopacify s)
    (\i -> NFS4Mixed <$> unopacify (BSC.take i s) <*> (Just <$> unopacify (BSC.drop (succ i) s)))
    $ BSC.elemIndexEnd '@' s
    
