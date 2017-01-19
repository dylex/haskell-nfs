{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
module Network.ONCRPC.Types
  ( XID
  , ProgNum
  , VersNum
  , ProcNum
  , Procedure(..)

  , FragmentHeader
  , maxFragmentSize
  , unFragmentHeader
  , mkFragmentHeader

  , Auth(..)
  , Call(..)
  , Reply(..)
  , Msg(..)
  ) where

import           Control.Monad (guard)
import           Data.Bits (Bits, finiteBitSize, bit, clearBit, setBit, testBit)
import qualified Data.ByteString as BS
import qualified Data.Serialize as S
import           Data.Word (Word32)
import           Network.Socket (htonl, ntohl)

import qualified Data.XDR as XDR
import           Data.XDR.Serial
import qualified Network.ONCRPC.Prot as RPC

type XID = Word32
type ProgNum = Word32
type VersNum = Word32
type ProcNum = Word32

-- |Description of a specific procedure, parameterized by argument and result types.
data Procedure a r = Procedure
  { procedureProg :: !ProgNum
  , procedureVers :: !VersNum
  , procedureProc :: !ProcNum
  }

-- |A raw RPC record fragment header, stored in network byte order.
type FragmentHeader = Word32

fragmentHeaderBit :: Int
fragmentHeaderBit = pred $ finiteBitSize (0 :: FragmentHeader)

maxFragmentSize :: (Bits i, Integral i) => i
maxFragmentSize = pred $ bit fragmentHeaderBit

unFragmentHeader :: Integral i => FragmentHeader -> (Bool, i)
unFragmentHeader w =
  (testBit w' fragmentHeaderBit, fromIntegral $ clearBit w' fragmentHeaderBit)
  where w' = ntohl w

mkFragmentHeader :: Integral i => Bool -> i -> FragmentHeader
mkFragmentHeader l n = htonl $ sb l $ fromIntegral n where
  sb True x = setBit x fragmentHeaderBit
  sb False x = x

-- |More translucent version of 'RPC.Opaque_auth' union (not expressible in XDR)
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

-- |'RPC.Call_body' with parameters
data Call a = Call
  { callProg :: !ProgNum
  , callVers :: !VersNum
  , callProc :: !ProcNum
  , callCred :: !Auth
  , callVerf :: !Auth
  , callArgs :: a
  }

splitCall :: Call a -> (RPC.Call_body, a)
splitCall Call{..} =
  ( RPC.Call_body
    { RPC.call_body'rpcvers = RPC.rPC_VERS
    , RPC.call_body'prog = callProg
    , RPC.call_body'vers = callVers
    , RPC.call_body'proc = callProc
    , RPC.call_body'cred = opacifyAuth callCred
    , RPC.call_body'verf = opacifyAuth callVerf
    }
  , callArgs
  )

getCall :: XDR.XDR a => RPC.Call_body -> S.Get (Call a)
getCall RPC.Call_body{..} = do
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

instance XDR.XDR a => XDR.XDR (Call a) where
  xdrType _ = "call_body_args"
  xdrPut = xdrPut . splitCall
  xdrGet = getCall =<< xdrGet

-- |'RPC.Reply_body' with results
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
  | ReplyFail -- ^Missing/corrupt response

splitReply :: Reply a -> (RPC.Reply_body, Maybe a)
splitReply (Reply v r) = 
  ( RPC.Reply_body'MSG_ACCEPTED
    $ RPC.Accepted_reply (opacifyAuth v) RPC.Accepted_reply_data'SUCCESS
  , Just r
  )
splitReply (ReplyError v e) =
  ( RPC.Reply_body'MSG_ACCEPTED
    $ RPC.Accepted_reply (opacifyAuth v) e
  , Nothing
  )
splitReply (ReplyRejected r) =
  ( RPC.Reply_body'MSG_DENIED r
  , Nothing
  )
splitReply ReplyFail = (error "splitReply ReplyFail", Nothing)

getReply :: XDR.XDR a => RPC.Reply_body -> S.Get (Reply a)
getReply (RPC.Reply_body'MSG_ACCEPTED (RPC.Accepted_reply v RPC.Accepted_reply_data'SUCCESS)) =
  Reply (unopacifyAuth v) <$> xdrGet
getReply (RPC.Reply_body'MSG_ACCEPTED (RPC.Accepted_reply v e)) =
  return $ ReplyError (unopacifyAuth v) e
getReply (RPC.Reply_body'MSG_DENIED r) =
  return $ ReplyRejected r

instance XDR.XDR a => XDR.XDR (Reply a) where
  xdrType _ = "reply_body_result"
  xdrPut ReplyFail = return ()
  xdrPut r = do
    xdrPut b
    mapM_ xdrPut a
    where (b, a) = splitReply r
  xdrGet = getReply =<< xdrGet

-- |'RPC.Rpc_msg' with arguments or results.
data Msg a r
  = MsgCall
    { msgXID :: XID
    , msgCall :: Call a
    }
  | MsgReply
    { msgXID :: XID
    , msgReply :: Reply r
    }

instance (XDR.XDR a, XDR.XDR r) => XDR.XDR (Msg a r) where
  xdrType _ = "rpc_msg_content"
  xdrPut (MsgCall x c) = xdrPut (RPC.Rpc_msg x $ RPC.Rpc_msg_body'CALL b, a)
    where (b, a) = splitCall c
  xdrPut (MsgReply x r) = do
    xdrPut $ RPC.Rpc_msg x $ RPC.Rpc_msg_body'REPLY b
    mapM_ xdrPut a
    where (b, a) = splitReply r
  xdrGet = do
    RPC.Rpc_msg x b <- xdrGet
    case b of
      RPC.Rpc_msg_body'CALL c -> MsgCall x <$> getCall c
      RPC.Rpc_msg_body'REPLY r -> MsgReply x <$> getReply r
