-- |Authentication mechanisms for RPC.

{-# LANGUAGE CPP #-}
module Network.ONCRPC.Auth
  ( Auth(..)
  , RPC.Authsys_parms(..)
  , opacifyAuth
  , unopacifyAuth
#ifdef VERSION_unix
  , getAuthUnix
#endif
  ) where

import qualified Data.ByteString.Char8 as BSC

#ifdef VERSION_unix
import           System.Posix.Process (getProcessID)
import           System.Posix.Unistd (getSystemID, nodeName)
import           System.Posix.User (getEffectiveUserID, getEffectiveGroupID, getGroups)
#endif

import qualified Network.ONCRPC.XDR as XDR
import           Network.ONCRPC.XDR.Array
import           Network.ONCRPC.XDR.Serial
import           Network.ONCRPC.XDR.Opaque
import qualified Network.ONCRPC.Prot as RPC

-- |More translucent version of 'RPC.Opaque_auth' union (not expressible in XDR)
data Auth
  = AuthNone
  | AuthSys !RPC.Authsys_parms
  | AuthOpaque !RPC.Opaque_auth
  deriving (Eq, Show)

opacifyAuth :: Auth -> RPC.Opaque_auth
opacifyAuth AuthNone    = RPC.Opaque_auth (xdrFromEnum RPC.AUTH_NONE) $ emptyBoundedLengthArray
opacifyAuth (AuthSys s) = RPC.Opaque_auth (xdrFromEnum RPC.AUTH_SYS)  $ toOpaque' s
opacifyAuth (AuthOpaque o) = o

unopacifyAuth :: RPC.Opaque_auth -> Auth
unopacifyAuth o@(RPC.Opaque_auth n b) = case xdrToEnum n of
  Just RPC.AUTH_NONE -> AuthNone
  Just RPC.AUTH_SYS | Just s <- fromOpaque b -> AuthSys s
  _ -> AuthOpaque o

instance XDR.XDR Auth where
  xdrType _ = "translucent_auth"
  xdrPut = xdrPut . opacifyAuth
  xdrGet = unopacifyAuth <$> xdrGet

#ifdef VERSION_unix
-- |Get the appropriate, effective AuthSys value for the current process.
-- You know, if you're into that sort of thing.
getAuthUnix :: IO Auth
getAuthUnix = do
  pid <- getProcessID
  sysid <- getSystemID
  uid <- getEffectiveUserID
  gid <- getEffectiveGroupID
  gids <- getGroups
  return $ AuthSys RPC.Authsys_parms
    { RPC.authsys_parms'stamp = fromIntegral pid
    , RPC.authsys_parms'machinename = boundLengthArray $ BSC.pack $ nodeName sysid
    , RPC.authsys_parms'uid = fromIntegral uid
    , RPC.authsys_parms'gid = fromIntegral gid
    , RPC.authsys_parms'gids = boundLengthArrayFromList $ map fromIntegral gids
    }
#endif
