{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Network.ONCRPC.Types
  ( XID
  , ProgNum
  , VersNum
  , ProcNum

  , FragmentHeader
  , mkFragmentHeader
  , unFragmentHeader
  , splitFragments

  , Auth(..)
  , Call(..)
  , Reply(..)
  ) where

import           Data.Bits (Bits, bit, clearBit, setBit, testBit)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import           Data.Word (Word32)
import           Foreign.Storable (Storable)
import           Network.Socket (htonl, ntohl)

import qualified Data.XDR as XDR
import qualified Network.ONCRPC.Prot as RPC

type XID = Word32
type ProgNum = Word32
type VersNum = Word32
type ProcNum = Word32

newtype FragmentHeader = FragmentHeader Word32
  deriving (Storable)

maxFragmentSize :: Bits i => i
maxFragmentSize = bit 31

mkFragmentHeader :: (Bits i, Integral i) => Bool -> i -> FragmentHeader
mkFragmentHeader l x
  | x < 0 || x >= maxFragmentSize = error "mkFragmentHeader"
  | l = r $ setBit w 31
  | otherwise = r w
  where
  w = fromIntegral x
  r = FragmentHeader . htonl

unFragmentHeader :: Integral i => FragmentHeader -> (Bool, i)
unFragmentHeader (FragmentHeader w) = (testBit w' 31, fromIntegral $ clearBit w' 31) where w' = ntohl w

splitFragments :: BSL.ByteString -> [(FragmentHeader, BS.ByteString)]
splitFragments b = f : if l then [] else splitFragments t where
  (h, t) = BSL.splitAt maxFragmentSize b
  h' = BSL.toStrict h
  l = BSL.null t
  f = (mkFragmentHeader l $ BS.length h', h')

data Auth
  = AuthNone
  | AuthSys !RPC.Authsys_parms
  | AuthOpaque !RPC.Opaque_auth
  | AuthPseudo
    { authPseudoFlavor :: !XDR.Int
    , authPseudoBody :: RPC.Opaque_auth_body
    }

authBody :: Auth -> RPC.Opaque_auth_body
authBody AuthNone = XDR.Opaque BS.empty
authBody (AuthSys s) = XDR.Opaque $ XDR.xdrSerialize s
authBody (AuthOpaque o) = RPC.opaque_auth'body o
authBody (AuthPseudo _ b) = b

instance XDR.XDR Auth where
  xdrType _ = "translucent_auth"
  xdrPut = XDR.xdrPutUnion
  xdrGet = XDR.xdrGetUnion
instance XDR.XDRUnion Auth where
  xdrDiscriminant AuthNone          = XDR.xdrFromEnum RPC.AUTH_NONE
  xdrDiscriminant (AuthSys _)       = XDR.xdrFromEnum RPC.AUTH_SYS
  xdrDiscriminant (AuthOpaque a)    = XDR.xdrFromEnum (RPC.opaque_auth'flavor a)
  xdrDiscriminant (AuthPseudo f _)  = f
  xdrPutUnionArm = XDR.xdrPut . authBody
  xdrGetUnionArm n = do
    o <- XDR.xdrGet
    case XDR.xdrToEnum n of
      Just RPC.AUTH_NONE -> return AuthNone
      Just RPC.AUTH_SYS -> AuthSys <$> XDR.xdrDeserialize (XDR.opaqueByteString o)
      Just f -> return $ AuthOpaque $ RPC.Opaque_auth f o
      Nothing -> return $ AuthPseudo n o

data Call a = Call
  { callProgNum :: !ProgNum
  , callVersNum :: !VersNum
  , callProcNum :: !ProcNum
  , callCred :: !Auth
  , callVerf :: !Auth
  , callArgs :: a
  }

data Reply a
  = Reply
    { replyVerf :: !Auth
    , replyResults :: a
    }
  | ReplyError
    { replyVerf :: !Auth
    , replyError :: !RPC.Accepted_reply_data -- ^non-SUCCESS only
    }
  | ReplyRejected
    { replyRejected :: !RPC.Rejected_reply
    }
