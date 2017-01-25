-- |Automatic code generation for NFSOp.
-- One of those cases where it ended up being more work to automate, what with all the exceptions.
{-# LANGUAGE TemplateHaskell #-}
module Network.NFS.V4.Ops.TH
  ( thNFSOps
  ) where

import           Data.List ((\\))
import qualified Data.Set as Set
import qualified Language.Haskell.TH as TH
import qualified Network.ONCRPC as RPC

import qualified Network.NFS.V4.Prot as NFS

thNFSOps :: [NFS.Nfs_opnum4] -> [NFS.Nfs_opnum4] -> TH.DecsQ
thNFSOps voidl excl = return $
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
        ])
    ops
  where
  r = TH.mkName "r"
  nfsOp = TH.ConT $ TH.mkName "NFSOp"
  nfsOpNum = TH.mkName "nfsOpNum"
  toNFSArgOp = TH.mkName "toNFSOpArg"
  fromNFSResOp = TH.mkName "fromNFSOpRes"
  opname op p s = TH.mkName $ p ++ (case show op of { ~('O':'P':'_':o) -> o }) ++ s
  voids = Set.fromDistinctAscList voidl
  ops = enumFromTo minBound maxBound \\ excl
