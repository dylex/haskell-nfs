-- |Automatic code generation for NFSOp.
-- One of those cases where it ended up being more work to automate, what with all the exceptions.
{-# LANGUAGE TemplateHaskell #-}
module Network.NFS.V4.Ops.TH
  ( thNFSOps
  ) where

import           Control.Monad (filterM)
import           Data.Char (toLower)
import qualified Data.Set as Set
import qualified Language.Haskell.TH as TH
import qualified Network.ONCRPC as RPC

import qualified Network.NFS.V4.Prot as NFS

-- Unpredictable field naming prefixes for v4.1 ops (why?)
opPrefix :: NFS.Nfs_opnum4 -> Maybe String
opPrefix NFS.OP_BACKCHANNEL_CTL = Just "bc"
opPrefix NFS.OP_BIND_CONN_TO_SESSION = Just "bcts"
opPrefix NFS.OP_EXCHANGE_ID = Just "ei"
opPrefix NFS.OP_CREATE_SESSION = Just "cs"
opPrefix NFS.OP_DESTROY_SESSION = Just "ds"
opPrefix NFS.OP_FREE_STATEID = Just "fs"
opPrefix NFS.OP_GET_DIR_DELEGATION = Just "gdd"
opPrefix NFS.OP_GETDEVICEINFO = Just "gdi"
opPrefix NFS.OP_GETDEVICELIST = Just "gdl"
opPrefix NFS.OP_LAYOUTCOMMIT = Just "loc"
opPrefix NFS.OP_LAYOUTGET = Just "log"
opPrefix NFS.OP_LAYOUTRETURN = Just "lor"
opPrefix NFS.OP_SEQUENCE = Just "s"
opPrefix NFS.OP_SET_SSV = Just "ss"
opPrefix NFS.OP_TEST_STATEID = Just "ts"
opPrefix NFS.OP_WANT_DELEGATION = Just "wd"
opPrefix NFS.OP_DESTROY_CLIENTID = Just "dc"
opPrefix NFS.OP_RECLAIM_COMPLETE = Just "rc"
opPrefix _ = Nothing

thNFSOps :: [NFS.Nfs_opnum4] -> TH.DecsQ
thNFSOps ops = do
  voidl <- filterM (\op -> do
      TH.DataConI _ argt _ <- TH.reify $ opname op "NFS.Nfs_argop4'OP_" ""
      return $ case argt of
        TH.ConT _ -> True
        ~(TH.AppT _ _) -> False)
    ops
  let voids = Set.fromDistinctAscList voidl
  return $
    concatMap (\op ->
        let args = opname op "" "4args" in
        [ TH.DataD [] args [] Nothing [TH.NormalC args []] []
        , TH.InstanceD Nothing [] (TH.ConT ''RPC.XDR `TH.AppT` TH.ConT args)
          [ TH.FunD 'RPC.xdrType [TH.Clause [TH.WildP] (TH.NormalB $ TH.LitE $ TH.StringL $ TH.nameBase args) []]
          , TH.FunD 'RPC.xdrPut [TH.Clause [TH.WildP] (TH.NormalB $ TH.VarE 'return `TH.AppE` TH.ConE '()) []]
          , TH.FunD 'RPC.xdrGet [TH.Clause [] (TH.NormalB $ TH.VarE 'return `TH.AppE` TH.ConE args) []]
          ]
        ])
      voidl
    ++ map (\op ->
        let opn = opname op
            void = Set.member op voids
            qual = if void then "" else "NFS." in
        TH.InstanceD Nothing [] (nfsOp `TH.AppT` TH.ConT (qual `opn` "4args") `TH.AppT` TH.ConT ("NFS." `opn` "4res"))
          [ TH.FunD nfsOpNum [TH.Clause [TH.WildP] (TH.NormalB $ TH.ConE $ "NFS.OP_" `opn` "") []]
          , TH.FunD toNFSArgOp [TH.Clause (if void then [TH.WildP] else []) (TH.NormalB $ TH.ConE $ "NFS.Nfs_argop4'OP_" `opn` "") []]
          , TH.FunD fromNFSResOp
            [ TH.Clause [TH.ConP ("NFS.Nfs_resop4'OP_" `opn` "") [TH.VarP r]] (TH.NormalB $ TH.ConE 'Just `TH.AppE` TH.VarE r) []
            , TH.Clause [TH.WildP] (TH.NormalB $ TH.ConE 'Nothing) []
            ]
          , TH.FunD nfsResOpStatus [TH.Clause [] (TH.NormalB $ TH.VarE $ opnamec toLower op "NFS." ("4res'" ++ maybe "" (++"r_") (opPrefix op) ++ "status")) []]
          ])
      ops
  where
  r = TH.mkName "r"
  nfsOp = TH.ConT $ TH.mkName "NFSOp"
  nfsOpNum = TH.mkName "nfsOpNum"
  toNFSArgOp = TH.mkName "toNFSArgOp"
  fromNFSResOp = TH.mkName "fromNFSResOp"
  nfsResOpStatus = TH.mkName "nfsResOpStatus"
  opname = opnamec id
  opnamec cf op p s = TH.mkName $ p ++ (case show op of { ~('O':'P':'_':h:t) -> cf h : t }) ++ s
