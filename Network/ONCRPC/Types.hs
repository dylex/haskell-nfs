{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
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

import           Control.Monad (guard)
import           Data.Bits (Bits, bit, clearBit, setBit, testBit)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import           Data.Word (Word32)
import           Foreign.Storable (Storable)
import           Network.Socket (htonl, ntohl)

import qualified Data.XDR as XDR
import           Data.XDR.Serial
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

opacifyAuth :: Auth -> RPC.Opaque_auth
opacifyAuth AuthNone    = RPC.Opaque_auth (xdrFromEnum RPC.AUTH_NONE) $ XDR.Opaque BS.empty
opacifyAuth (AuthSys s) = RPC.Opaque_auth (xdrFromEnum RPC.AUTH_SYS)  $ XDR.Opaque $ xdrSerialize s
opacifyAuth (AuthOpaque o) = o

unopacifyAuth :: RPC.Opaque_auth -> Auth
unopacifyAuth o@(RPC.Opaque_auth n b) = case xdrToEnum n of
  Just RPC.AUTH_NONE -> AuthNone
  Just RPC.AUTH_SYS | Just s <- xdrDeserialize (XDR.opaqueByteString b) -> AuthSys s
  _ -> AuthOpaque o

instance XDR.XDR Auth where
  xdrType _ = "translucent_auth"
  xdrPut = xdrPut . opacifyAuth
  xdrGet = unopacifyAuth <$> xdrGet

data Call a = Call
  { callProg :: !ProgNum
  , callVers :: !VersNum
  , callProc :: !ProcNum
  , callCred :: !Auth
  , callVerf :: !Auth
  , callArgs :: a
  }

instance XDR.XDR a => XDR.XDR (Call a) where
  xdrType _ = "call_body_args"
  xdrPut Call{..} = do
    xdrPut RPC.Call_body
      { RPC.call_body'rpcvers = RPC.rPC_VERS
      , RPC.call_body'prog = callProg
      , RPC.call_body'vers = callVers
      , RPC.call_body'proc = callProc
      , RPC.call_body'cred = opacifyAuth callCred
      , RPC.call_body'verf = opacifyAuth callVerf
      }
    xdrPut callArgs
  xdrGet = do
    RPC.Call_body{..} <- xdrGet
    guard $ call_body'rpcvers == RPC.rPC_VERS
    a <- xdrGet
    return Call
      { callProg = call_body'prog
      , callVers = call_body'vers
      , callProc = call_body'proc
      , callCred = unopacifyAuth call_body'cred
      , callVerf = unopacifyAuth call_body'verf
      , callArgs = a
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

instance XDR.XDR a => XDR.XDR (Reply a) where
  xdrType _ = "reply_body_result"
  xdrPut (Reply v r) = do
    xdrPut $ RPC.Reply_body'MSG_ACCEPTED
      $ RPC.Accepted_reply (opacifyAuth v) RPC.Accepted_reply_data'SUCCESS
    xdrPut r
  xdrPut (ReplyError v e) =
    xdrPut $ RPC.Reply_body'MSG_ACCEPTED
      $ RPC.Accepted_reply (opacifyAuth v) e
  xdrPut (ReplyRejected r) =
    xdrPut $ RPC.Reply_body'MSG_DENIED r
  xdrGet = do
    b <- xdrGet
    case b of
      RPC.Reply_body'MSG_ACCEPTED (RPC.Accepted_reply v RPC.Accepted_reply_data'SUCCESS) -> do
        r <- xdrGet
        return $ Reply (unopacifyAuth v) r
      RPC.Reply_body'MSG_ACCEPTED (RPC.Accepted_reply v e) ->
        return $ ReplyError (unopacifyAuth v) e
      RPC.Reply_body'MSG_DENIED r ->
        return $ ReplyRejected r