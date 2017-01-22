-- |RPC protocol description.
-- Generated from rpc.x by https://github.com/dylex/oncrpc
{-# LANGUAGE DataKinds, MultiParamTypeClasses, TypeSynonymInstances
  #-}
module Network.ONCRPC.Prot where
import qualified Prelude
import qualified Control.Applicative
import qualified Network.ONCRPC.XDR as XDR

rPC_VERS :: Prelude.Integral a => a
rPC_VERS = 2

data Auth_flavor = AUTH_NONE
                 | AUTH_SYS
                 | AUTH_SHORT
                 | AUTH_DH
                 | AUTH_KERB
                 | AUTH_RSA
                 | RPCSEC_GSS
                 deriving (Prelude.Eq, Prelude.Ord, Prelude.Enum, Prelude.Bounded,
                           Prelude.Show)

instance XDR.XDR Auth_flavor where
  xdrType _ = "Auth_flavor"
  xdrPut = XDR.xdrPutEnum
  xdrGet = XDR.xdrGetEnum

instance XDR.XDREnum Auth_flavor where
  xdrFromEnum AUTH_NONE = 0
  xdrFromEnum AUTH_SYS = 1
  xdrFromEnum AUTH_SHORT = 2
  xdrFromEnum AUTH_DH = 3
  xdrFromEnum AUTH_KERB = 4
  xdrFromEnum AUTH_RSA = 5
  xdrFromEnum RPCSEC_GSS = 6
  xdrToEnum 0 = Prelude.return AUTH_NONE
  xdrToEnum 1 = Prelude.return AUTH_SYS
  xdrToEnum 2 = Prelude.return AUTH_SHORT
  xdrToEnum 3 = Prelude.return AUTH_DH
  xdrToEnum 4 = Prelude.return AUTH_KERB
  xdrToEnum 5 = Prelude.return AUTH_RSA
  xdrToEnum 6 = Prelude.return RPCSEC_GSS
  xdrToEnum _ = Prelude.fail "invalid Auth_flavor"

type Opaque_auth_body = XDR.Opaque 400

data Opaque_auth = Opaque_auth{opaque_auth'flavor :: !XDR.Int,
                               opaque_auth'body :: !Opaque_auth_body}
                 deriving (Prelude.Eq, Prelude.Show)

instance XDR.XDR Opaque_auth where
  xdrType _ = "Opaque_auth"
  xdrPut _x
    = XDR.xdrPut (opaque_auth'flavor _x) Control.Applicative.*>
        XDR.xdrPut (opaque_auth'body _x)
  xdrGet
    = Control.Applicative.pure Opaque_auth Control.Applicative.<*>
        XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet

data Msg_type = CALL
              | REPLY
              deriving (Prelude.Eq, Prelude.Ord, Prelude.Enum, Prelude.Bounded,
                        Prelude.Show)

instance XDR.XDR Msg_type where
  xdrType _ = "Msg_type"
  xdrPut = XDR.xdrPutEnum
  xdrGet = XDR.xdrGetEnum

instance XDR.XDREnum Msg_type where
  xdrFromEnum CALL = 0
  xdrFromEnum REPLY = 1
  xdrToEnum 0 = Prelude.return CALL
  xdrToEnum 1 = Prelude.return REPLY
  xdrToEnum _ = Prelude.fail "invalid Msg_type"

data Reply_stat = MSG_ACCEPTED
                | MSG_DENIED
                deriving (Prelude.Eq, Prelude.Ord, Prelude.Enum, Prelude.Bounded,
                          Prelude.Show)

instance XDR.XDR Reply_stat where
  xdrType _ = "Reply_stat"
  xdrPut = XDR.xdrPutEnum
  xdrGet = XDR.xdrGetEnum

instance XDR.XDREnum Reply_stat where
  xdrFromEnum MSG_ACCEPTED = 0
  xdrFromEnum MSG_DENIED = 1
  xdrToEnum 0 = Prelude.return MSG_ACCEPTED
  xdrToEnum 1 = Prelude.return MSG_DENIED
  xdrToEnum _ = Prelude.fail "invalid Reply_stat"

data Accept_stat = SUCCESS
                 | PROG_UNAVAIL
                 | PROG_MISMATCH
                 | PROC_UNAVAIL
                 | GARBAGE_ARGS
                 | SYSTEM_ERR
                 deriving (Prelude.Eq, Prelude.Ord, Prelude.Enum, Prelude.Bounded,
                           Prelude.Show)

instance XDR.XDR Accept_stat where
  xdrType _ = "Accept_stat"
  xdrPut = XDR.xdrPutEnum
  xdrGet = XDR.xdrGetEnum

instance XDR.XDREnum Accept_stat where
  xdrFromEnum SUCCESS = 0
  xdrFromEnum PROG_UNAVAIL = 1
  xdrFromEnum PROG_MISMATCH = 2
  xdrFromEnum PROC_UNAVAIL = 3
  xdrFromEnum GARBAGE_ARGS = 4
  xdrFromEnum SYSTEM_ERR = 5
  xdrToEnum 0 = Prelude.return SUCCESS
  xdrToEnum 1 = Prelude.return PROG_UNAVAIL
  xdrToEnum 2 = Prelude.return PROG_MISMATCH
  xdrToEnum 3 = Prelude.return PROC_UNAVAIL
  xdrToEnum 4 = Prelude.return GARBAGE_ARGS
  xdrToEnum 5 = Prelude.return SYSTEM_ERR
  xdrToEnum _ = Prelude.fail "invalid Accept_stat"

data Reject_stat = RPC_MISMATCH
                 | AUTH_ERROR
                 deriving (Prelude.Eq, Prelude.Ord, Prelude.Enum, Prelude.Bounded,
                           Prelude.Show)

instance XDR.XDR Reject_stat where
  xdrType _ = "Reject_stat"
  xdrPut = XDR.xdrPutEnum
  xdrGet = XDR.xdrGetEnum

instance XDR.XDREnum Reject_stat where
  xdrFromEnum RPC_MISMATCH = 0
  xdrFromEnum AUTH_ERROR = 1
  xdrToEnum 0 = Prelude.return RPC_MISMATCH
  xdrToEnum 1 = Prelude.return AUTH_ERROR
  xdrToEnum _ = Prelude.fail "invalid Reject_stat"

data Auth_stat = AUTH_OK
               | AUTH_BADCRED
               | AUTH_REJECTEDCRED
               | AUTH_BADVERF
               | AUTH_REJECTEDVERF
               | AUTH_TOOWEAK
               | AUTH_INVALIDRESP
               | AUTH_FAILED
               | AUTH_KERB_GENERIC
               | AUTH_TIMEEXPIRE
               | AUTH_TKT_FILE
               | AUTH_DECODE
               | AUTH_NET_ADDR
               | RPCSEC_GSS_CREDPROBLEM
               | RPCSEC_GSS_CTXPROBLEM
               deriving (Prelude.Eq, Prelude.Ord, Prelude.Enum, Prelude.Bounded,
                         Prelude.Show)

instance XDR.XDR Auth_stat where
  xdrType _ = "Auth_stat"
  xdrPut = XDR.xdrPutEnum
  xdrGet = XDR.xdrGetEnum

instance XDR.XDREnum Auth_stat where
  xdrFromEnum AUTH_OK = 0
  xdrFromEnum AUTH_BADCRED = 1
  xdrFromEnum AUTH_REJECTEDCRED = 2
  xdrFromEnum AUTH_BADVERF = 3
  xdrFromEnum AUTH_REJECTEDVERF = 4
  xdrFromEnum AUTH_TOOWEAK = 5
  xdrFromEnum AUTH_INVALIDRESP = 6
  xdrFromEnum AUTH_FAILED = 7
  xdrFromEnum AUTH_KERB_GENERIC = 8
  xdrFromEnum AUTH_TIMEEXPIRE = 9
  xdrFromEnum AUTH_TKT_FILE = 10
  xdrFromEnum AUTH_DECODE = 11
  xdrFromEnum AUTH_NET_ADDR = 12
  xdrFromEnum RPCSEC_GSS_CREDPROBLEM = 13
  xdrFromEnum RPCSEC_GSS_CTXPROBLEM = 14
  xdrToEnum 0 = Prelude.return AUTH_OK
  xdrToEnum 1 = Prelude.return AUTH_BADCRED
  xdrToEnum 2 = Prelude.return AUTH_REJECTEDCRED
  xdrToEnum 3 = Prelude.return AUTH_BADVERF
  xdrToEnum 4 = Prelude.return AUTH_REJECTEDVERF
  xdrToEnum 5 = Prelude.return AUTH_TOOWEAK
  xdrToEnum 6 = Prelude.return AUTH_INVALIDRESP
  xdrToEnum 7 = Prelude.return AUTH_FAILED
  xdrToEnum 8 = Prelude.return AUTH_KERB_GENERIC
  xdrToEnum 9 = Prelude.return AUTH_TIMEEXPIRE
  xdrToEnum 10 = Prelude.return AUTH_TKT_FILE
  xdrToEnum 11 = Prelude.return AUTH_DECODE
  xdrToEnum 12 = Prelude.return AUTH_NET_ADDR
  xdrToEnum 13 = Prelude.return RPCSEC_GSS_CREDPROBLEM
  xdrToEnum 14 = Prelude.return RPCSEC_GSS_CTXPROBLEM
  xdrToEnum _ = Prelude.fail "invalid Auth_stat"

data Rpc_msg = Rpc_msg{rpc_msg'xid :: !XDR.UnsignedInt,
                       rpc_msg'body :: !Rpc_msg_body}
             deriving (Prelude.Eq, Prelude.Show)

instance XDR.XDR Rpc_msg where
  xdrType _ = "Rpc_msg"
  xdrPut _x
    = XDR.xdrPut (rpc_msg'xid _x) Control.Applicative.*>
        XDR.xdrPut (rpc_msg'body _x)
  xdrGet
    = Control.Applicative.pure Rpc_msg Control.Applicative.<*>
        XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet

data Rpc_msg_body = Rpc_msg_body'CALL{rpc_msg_body'cbody ::
                                      !Call_body}
                  | Rpc_msg_body'REPLY{rpc_msg_body'rbody :: !Reply_body}
                  deriving (Prelude.Eq, Prelude.Show)

rpc_msg_body'mtype :: Rpc_msg_body -> Msg_type
rpc_msg_body'mtype = XDR.xdrToEnum' Prelude.. XDR.xdrDiscriminant

instance XDR.XDR Rpc_msg_body where
  xdrType _ = "Rpc_msg_body"
  xdrPut = XDR.xdrPutUnion
  xdrGet = XDR.xdrGetUnion

instance XDR.XDRUnion Rpc_msg_body where
  xdrDiscriminant Rpc_msg_body'CALL{} = 0
  xdrDiscriminant Rpc_msg_body'REPLY{} = 1
  xdrPutUnionArm _x@Rpc_msg_body'CALL{}
    = XDR.xdrPut (rpc_msg_body'cbody _x)
  xdrPutUnionArm _x@Rpc_msg_body'REPLY{}
    = XDR.xdrPut (rpc_msg_body'rbody _x)
  xdrGetUnionArm 0
    = Control.Applicative.pure Rpc_msg_body'CALL
        Control.Applicative.<*> XDR.xdrGet
  xdrGetUnionArm 1
    = Control.Applicative.pure Rpc_msg_body'REPLY
        Control.Applicative.<*> XDR.xdrGet
  xdrGetUnionArm _c
    = Prelude.fail "invalid Rpc_msg_body discriminant"

data Call_body = Call_body{call_body'rpcvers :: !XDR.UnsignedInt,
                           call_body'prog :: !XDR.UnsignedInt,
                           call_body'vers :: !XDR.UnsignedInt,
                           call_body'proc :: !XDR.UnsignedInt, call_body'cred :: !Opaque_auth,
                           call_body'verf :: !Opaque_auth}
               deriving (Prelude.Eq, Prelude.Show)

instance XDR.XDR Call_body where
  xdrType _ = "Call_body"
  xdrPut _x
    = XDR.xdrPut (call_body'rpcvers _x) Control.Applicative.*>
        XDR.xdrPut (call_body'prog _x)
        Control.Applicative.*> XDR.xdrPut (call_body'vers _x)
        Control.Applicative.*> XDR.xdrPut (call_body'proc _x)
        Control.Applicative.*> XDR.xdrPut (call_body'cred _x)
        Control.Applicative.*> XDR.xdrPut (call_body'verf _x)
  xdrGet
    = Control.Applicative.pure Call_body Control.Applicative.<*>
        XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet

data Reply_body = Reply_body'MSG_ACCEPTED{reply_body'areply ::
                                          !Accepted_reply}
                | Reply_body'MSG_DENIED{reply_body'rreply :: !Rejected_reply}
                deriving (Prelude.Eq, Prelude.Show)

reply_body'stat :: Reply_body -> Reply_stat
reply_body'stat = XDR.xdrToEnum' Prelude.. XDR.xdrDiscriminant

instance XDR.XDR Reply_body where
  xdrType _ = "Reply_body"
  xdrPut = XDR.xdrPutUnion
  xdrGet = XDR.xdrGetUnion

instance XDR.XDRUnion Reply_body where
  xdrDiscriminant Reply_body'MSG_ACCEPTED{} = 0
  xdrDiscriminant Reply_body'MSG_DENIED{} = 1
  xdrPutUnionArm _x@Reply_body'MSG_ACCEPTED{}
    = XDR.xdrPut (reply_body'areply _x)
  xdrPutUnionArm _x@Reply_body'MSG_DENIED{}
    = XDR.xdrPut (reply_body'rreply _x)
  xdrGetUnionArm 0
    = Control.Applicative.pure Reply_body'MSG_ACCEPTED
        Control.Applicative.<*> XDR.xdrGet
  xdrGetUnionArm 1
    = Control.Applicative.pure Reply_body'MSG_DENIED
        Control.Applicative.<*> XDR.xdrGet
  xdrGetUnionArm _c = Prelude.fail "invalid Reply_body discriminant"

data Accepted_reply = Accepted_reply{accepted_reply'verf ::
                                     !Opaque_auth,
                                     accepted_reply'reply_data :: !Accepted_reply_data}
                    deriving (Prelude.Eq, Prelude.Show)

instance XDR.XDR Accepted_reply where
  xdrType _ = "Accepted_reply"
  xdrPut _x
    = XDR.xdrPut (accepted_reply'verf _x) Control.Applicative.*>
        XDR.xdrPut (accepted_reply'reply_data _x)
  xdrGet
    = Control.Applicative.pure Accepted_reply Control.Applicative.<*>
        XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet

data Accepted_reply_data = Accepted_reply_data'SUCCESS{}
                         | Accepted_reply_data'PROG_MISMATCH{accepted_reply_data'mismatch_info'low
                                                             :: !XDR.UnsignedInt,
                                                             accepted_reply_data'mismatch_info'high
                                                             :: !XDR.UnsignedInt}
                         | Accepted_reply_data'default{accepted_reply_data'stat' ::
                                                       !Accept_stat}
                         deriving (Prelude.Eq, Prelude.Show)

accepted_reply_data'stat :: Accepted_reply_data -> Accept_stat
accepted_reply_data'stat
  = XDR.xdrToEnum' Prelude.. XDR.xdrDiscriminant

instance XDR.XDR Accepted_reply_data where
  xdrType _ = "Accepted_reply_data"
  xdrPut = XDR.xdrPutUnion
  xdrGet = XDR.xdrGetUnion

instance XDR.XDRUnion Accepted_reply_data where
  xdrDiscriminant Accepted_reply_data'SUCCESS{} = 0
  xdrDiscriminant Accepted_reply_data'PROG_MISMATCH{} = 2
  xdrDiscriminant
    Accepted_reply_data'default{accepted_reply_data'stat' = x}
    = XDR.xdrFromEnum x
  xdrPutUnionArm _x@Accepted_reply_data'SUCCESS{}
    = Control.Applicative.pure ()
  xdrPutUnionArm _x@Accepted_reply_data'PROG_MISMATCH{}
    = XDR.xdrPut (accepted_reply_data'mismatch_info'low _x)
        Control.Applicative.*>
        XDR.xdrPut (accepted_reply_data'mismatch_info'high _x)
  xdrPutUnionArm _x@Accepted_reply_data'default{}
    = Control.Applicative.pure ()
  xdrGetUnionArm 0
    = Control.Applicative.pure Accepted_reply_data'SUCCESS
  xdrGetUnionArm 2
    = Control.Applicative.pure Accepted_reply_data'PROG_MISMATCH
        Control.Applicative.<*> XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet
  xdrGetUnionArm _c
    = Accepted_reply_data'default Control.Applicative.<$>
        XDR.xdrToEnum _c

data Rejected_reply = Rejected_reply'RPC_MISMATCH{rejected_reply'mismatch_info'low
                                                  :: !XDR.UnsignedInt,
                                                  rejected_reply'mismatch_info'high ::
                                                  !XDR.UnsignedInt}
                    | Rejected_reply'AUTH_ERROR{rejected_reply'auth_stat :: !Auth_stat}
                    deriving (Prelude.Eq, Prelude.Show)

rejected_reply'stat :: Rejected_reply -> Reject_stat
rejected_reply'stat = XDR.xdrToEnum' Prelude.. XDR.xdrDiscriminant

instance XDR.XDR Rejected_reply where
  xdrType _ = "Rejected_reply"
  xdrPut = XDR.xdrPutUnion
  xdrGet = XDR.xdrGetUnion

instance XDR.XDRUnion Rejected_reply where
  xdrDiscriminant Rejected_reply'RPC_MISMATCH{} = 0
  xdrDiscriminant Rejected_reply'AUTH_ERROR{} = 1
  xdrPutUnionArm _x@Rejected_reply'RPC_MISMATCH{}
    = XDR.xdrPut (rejected_reply'mismatch_info'low _x)
        Control.Applicative.*>
        XDR.xdrPut (rejected_reply'mismatch_info'high _x)
  xdrPutUnionArm _x@Rejected_reply'AUTH_ERROR{}
    = XDR.xdrPut (rejected_reply'auth_stat _x)
  xdrGetUnionArm 0
    = Control.Applicative.pure Rejected_reply'RPC_MISMATCH
        Control.Applicative.<*> XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet
  xdrGetUnionArm 1
    = Control.Applicative.pure Rejected_reply'AUTH_ERROR
        Control.Applicative.<*> XDR.xdrGet
  xdrGetUnionArm _c
    = Prelude.fail "invalid Rejected_reply discriminant"

data Authsys_parms = Authsys_parms{authsys_parms'stamp ::
                                   !XDR.UnsignedInt,
                                   authsys_parms'machinename :: !(XDR.String 255),
                                   authsys_parms'uid :: !XDR.UnsignedInt,
                                   authsys_parms'gid :: !XDR.UnsignedInt,
                                   authsys_parms'gids :: !(XDR.Array 16 XDR.UnsignedInt)}
                   deriving (Prelude.Eq, Prelude.Show)

instance XDR.XDR Authsys_parms where
  xdrType _ = "Authsys_parms"
  xdrPut _x
    = XDR.xdrPut (authsys_parms'stamp _x) Control.Applicative.*>
        XDR.xdrPut (authsys_parms'machinename _x)
        Control.Applicative.*> XDR.xdrPut (authsys_parms'uid _x)
        Control.Applicative.*> XDR.xdrPut (authsys_parms'gid _x)
        Control.Applicative.*> XDR.xdrPut (authsys_parms'gids _x)
  xdrGet
    = Control.Applicative.pure Authsys_parms Control.Applicative.<*>
        XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet
        Control.Applicative.<*> XDR.xdrGet
