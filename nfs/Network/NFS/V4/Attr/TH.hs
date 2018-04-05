-- |Automatic code generation for Attr
{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
module Network.NFS.V4.Attr.TH
  ( thAttr
  ) where

import           Data.Char (toUpper, toLower)
import qualified Data.Serialize as S
import qualified Language.Haskell.TH as TH
import qualified Network.ONCRPC as RPC

#if MIN_VERSION_template_haskell(2,11,0)
#define Nothing_2_11 Nothing
#else
#define Nothing_2_11
#endif

toCamel :: String -> String
toCamel (u:s) = toUpper u : tc s where
  tc ('_':r) = toCamel r
  tc (l:r) = toLower l : tc r
  tc [] = []
toCamel [] = []

thAttr :: [String] -> TH.DecsQ
thAttr attrs = return
  [ TH.DataD [] nfstype [] Nothing_2_11
    (forAttrs $ \a -> TH.NormalC (ntype a) [])
    (
#if MIN_VERSION_template_haskell(2,11,0)
#if MIN_VERSION_template_haskell(2,12,0)
      return $ TH.DerivClause Nothing $
#endif
      map TH.ConT
#endif
      [''Eq, ''Ord, ''Enum, ''Bounded, ''Show])
  , TH.InstanceD Nothing_2_11 [] (TH.ConT ''RPC.XDREnum `TH.AppT` TH.ConT nfstype)
    [ TH.FunD 'RPC.xdrFromEnum $ forAttrs $ \a ->
        TH.Clause [TH.ConP (ntype a) []]
          (TH.NormalB $ ftype a) []
    , TH.FunD 'RPC.xdrToEnum [TH.Clause [TH.VarP x]
        (TH.GuardedB $ forAttrs (\a ->
          ( TH.NormalG $ TH.InfixE (Just $ TH.VarE x) (TH.VarE '(==)) (Just $ ftype a)
          , TH.VarE 'return `TH.AppE` TH.ConE (ntype a)))
        ++ [(TH.NormalG $ TH.VarE 'otherwise
          , TH.VarE 'fail `TH.AppE` TH.LitE (TH.StringL $ "invalid " ++ TH.nameBase nfstype))])
        []]
    ]
  , TH.DataD [] nfsval [] Nothing_2_11
    (forAttrs $ \a -> TH.NormalC (nval a) [(
#if MIN_VERSION_template_haskell(2,11,0)
      TH.Bang TH.NoSourceUnpackedness TH.SourceStrict
#else
      TH.IsStrict
#endif
      , fval a)])
    (
#if MIN_VERSION_template_haskell(2,11,0)
#if MIN_VERSION_template_haskell(2,12,0)
      return $ TH.DerivClause Nothing $
#endif
      map TH.ConT
#endif
      [''Eq, ''Show])
  , TH.SigD (TH.mkName "attrType") $ TH.ArrowT `TH.AppT` TH.ConT nfsval `TH.AppT` TH.ConT nfstype
  , TH.FunD (TH.mkName "attrType") $ forAttrs $ \a ->
      TH.Clause [TH.ConP (nval a) [TH.WildP]] (TH.NormalB $ TH.ConE $ ntype a) []
  , TH.SigD (TH.mkName "putAttr") $ TH.ArrowT `TH.AppT` TH.ConT nfsval `TH.AppT` TH.ConT ''S.Put
  , TH.FunD (TH.mkName "putAttr") $ forAttrs $ \a ->
      TH.Clause [TH.ConP (nval a) [TH.VarP x]] (TH.NormalB $ TH.VarE 'RPC.xdrPut `TH.AppE` TH.VarE x) []
  , TH.SigD (TH.mkName "getAttr") $ TH.ArrowT `TH.AppT` TH.ConT nfstype `TH.AppT` (TH.ConT ''S.Get `TH.AppT` TH.ConT nfsval)
  , TH.FunD (TH.mkName "getAttr") $ forAttrs $ \a ->
      TH.Clause [TH.ConP (ntype a) []] (TH.NormalB $ TH.InfixE (Just $ TH.ConE (nval a)) (TH.VarE '(<$>)) (Just $ TH.VarE 'RPC.xdrGet)) []
  ]
  where
  forAttrs = flip map attrs
  x = TH.mkName "x"
  nfstype = TH.mkName "AttrType"
  ntype = TH.mkName . ("AttrType" ++) . toCamel
  ftype = TH.VarE . TH.mkName . ("NFS.fATTR4_" ++) . map toUpper
  nfsval = TH.mkName "AttrVal"
  nval = TH.mkName . ("AttrVal" ++) . toCamel
  fval = TH.ConT . TH.mkName . ("NFS.Fattr4_" ++) . map toLower
