-- |Automatic code generation for Op.
-- One of those cases where it ended up being more work to automate, what with all the exceptions.
{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
module Network.NFS.V4.Ops.TH
  ( thOps
  ) where

import           Data.List ((\\))
import qualified Data.Set as Set
import qualified Language.Haskell.TH as TH
import qualified Network.ONCRPC as RPC

import qualified Network.NFS.V4.Prot as NFS

#if MIN_VERSION_template_haskell(2,11,0)
#define Nothing_2_11 Nothing
#else
#define Nothing_2_11
#endif

thOps :: [NFS.Nfs_opnum4] -> [NFS.Nfs_opnum4] -> TH.DecsQ
thOps voidl excl = return $
  concatMap (\o ->
      let args = opname o "" "4args" in
      [ TH.DataD [] args [] Nothing_2_11 [TH.NormalC args []] []
      , TH.InstanceD Nothing_2_11 [] (TH.ConT ''RPC.XDR `TH.AppT` TH.ConT args)
        [ TH.FunD 'RPC.xdrType [TH.Clause [TH.WildP] (TH.NormalB $ TH.LitE $ TH.StringL $ TH.nameBase args) []]
        , TH.FunD 'RPC.xdrPut [TH.Clause [TH.WildP] (TH.NormalB $ TH.VarE 'return `TH.AppE` TH.ConE '()) []]
        , TH.FunD 'RPC.xdrGet [TH.Clause [] (TH.NormalB $ TH.VarE 'return `TH.AppE` TH.ConE args) []]
        ]
      ])
    voidl
  ++ map (\o ->
      let opn = opname o
          void = Set.member o voids
          qual = if void then "" else "NFS." in
      TH.InstanceD Nothing_2_11 [] (op `TH.AppT` TH.ConT (qual `opn` "4args") `TH.AppT` TH.ConT ("NFS." `opn` "4res"))
        [ TH.FunD opNum [TH.Clause [TH.WildP] (TH.NormalB $ TH.ConE $ "NFS.OP_" `opn` "") []]
        , TH.FunD toArgOp [TH.Clause (if void then [TH.WildP] else []) (TH.NormalB $ TH.ConE $ "NFS.Nfs_argop4'OP_" `opn` "") []]
        , TH.FunD fromResOp
          [ TH.Clause [TH.ConP ("NFS.Nfs_resop4'OP_" `opn` "") [TH.VarP r]] (TH.NormalB $ TH.ConE 'Just `TH.AppE` TH.VarE r) []
          , TH.Clause [TH.WildP] (TH.NormalB $ TH.ConE 'Nothing) []
          ]
        ])
    ops
  where
  r = TH.mkName "r"
  op = TH.ConT $ TH.mkName "Op"
  opNum = TH.mkName "opNum"
  toArgOp = TH.mkName "toOpArg"
  fromResOp = TH.mkName "fromOpRes"
  opname o p s = TH.mkName $ p ++ (case show o of { ~('O':'P':'_':x) -> x }) ++ s
  voids = Set.fromDistinctAscList voidl
  ops = enumFromTo minBound maxBound \\ excl
