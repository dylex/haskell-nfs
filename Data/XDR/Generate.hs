
{-# LANGUAGE ViewPatterns #-}
module Data.XDR.Generate
  ( generate
  ) where

import           Control.Arrow ((***))
import           Data.Char (toLower, toUpper)
import qualified Data.Map as Map
import qualified Language.Haskell.Exts.Build as HS
import qualified Language.Haskell.Exts.Syntax as HS

import           Data.XDR.Specification
import qualified Data.XDR.Parse as XDR

xdrModule :: HS.ModuleName ()
xdrModule = HS.ModuleName () "XDR"

unQual :: String -> HS.QName ()
unQual = HS.UnQual () . HS.name

identifierName :: XDR.Scope -> Bool -> Identifier -> HS.Name ()
identifierName s u i@(~(h:r)) = HS.name
  $ (if any XDR.bindingInitCaseConflict $ Map.lookup i s then (++"'") else id)
  $ (if u then toUpper else toLower) h : r

constantType :: HS.Type ()
constantType = HS.TyForall ()
  Nothing
  (Just $ HS.CxSingle () $ HS.ClassA () (unQual "Integral") [t])
  t
  where
  t = HS.TyVar () $ HS.name "a"

primType :: TypeSpecifier -> Maybe String
primType TypeInt           = Just "Int"
primType TypeUnsignedInt   = Just "UnsignedInt"
primType TypeHyper         = Just "Hyper"
primType TypeUnsignedHyper = Just "UnsignedHyper"
primType TypeFloat         = Just "Float"
primType TypeDouble        = Just "Double"
primType TypeQuadruple     = Just "Quadruple"
primType TypeBool          = Just "Bool"
primType _                 = Nothing

instDecl :: String -> HS.Name () -> [HS.Decl ()] -> HS.Decl ()
instDecl c t = HS.InstDecl () Nothing (HS.IRule () Nothing Nothing (HS.IHApp () (HS.IHCon () (unQual c)) (HS.TyCon () (HS.UnQual () t)))) . Just . map (HS.InsDecl ())

definition :: XDR.Scope -> Definition -> [HS.Decl ()]
definition scope (Definition n (Constant v)) =
  [ HS.TypeSig () [hn] constantType
  , HS.nameBind hn (HS.intE v)
  ] where
  hn = identifierName scope False n
definition scope (Definition n (TypeDef (TypeSingle (primType -> Just t)))) =
  [ HS.TypeDecl () (HS.DHead () hn) $ HS.TyCon () $ HS.Qual () xdrModule $ HS.name t
  ] where
  hn = identifierName scope True n
definition scope (Definition n (TypeDef (TypeSingle (TypeEnum (EnumBody el))))) =
  [ HS.DataDecl () (HS.DataType ()) Nothing (HS.DHead () hn)
    (map (HS.QualConDecl () Nothing Nothing . flip (HS.ConDecl ()) [] . fst) hel) Nothing
  , instDecl "Enum" hn
    [ HS.FunBind () $ map (\(i,v) ->
        HS.Match () (HS.name "fromEnum") [HS.pApp i []] (HS.UnGuardedRhs () $ HS.intE v) Nothing)
      hel
    , HS.nameBind (HS.name "toEnum") (HS.var $ HS.name "xdrToEnum")
    ]
  , instDecl "XDR" hn
    [ HS.nameBind (HS.name "xdrPut") (HS.var $ HS.name "xdrPutEnum")
    , HS.nameBind (HS.name "xdrGet") (HS.var $ HS.name "xdrGetEnum")
    ]
  , instDecl "XDREnum" hn
    [ HS.FunBind () $ map (\(i,v) ->
        HS.Match () (HS.name "toXDREnum") [HS.intP v] (HS.UnGuardedRhs () $ HS.app (HS.Con () $ unQual "Just") $ HS.Con () $ HS.UnQual () i) Nothing)
      hel ++
      [ HS.Match () (HS.name "toXDREnum") [HS.PWildCard ()] (HS.UnGuardedRhs () $ HS.Con () $ unQual "Nothing") Nothing]
    ]
  ] where
  hn = identifierName scope True n
  hel = map (identifierName scope True *** toInteger) el
definition scope (Definition n (TypeDef (TypeSingle (TypeIdentifier t)))) =
  [ HS.TypeDecl () (HS.DHead () hn) $ HS.TyCon () $ HS.UnQual () ht
  ] where
  hn = identifierName scope True n
  ht = identifierName scope True t
definition _ _ = []

generate :: XDR.Scope -> String -> Specification -> HS.Module ()
generate s n l = HS.Module ()
  (Just $ HS.ModuleHead () (HS.ModuleName () n) Nothing Nothing)
  []
  [ HS.ImportDecl () (HS.ModuleName () "Prelude") False False False Nothing Nothing
    $ Just $ HS.ImportSpecList () False $ map (HS.IVar () . HS.name) ["Integral"] ++ map (HS.IThingAll () . HS.name) ["Enum", "Maybe"]
  , HS.ImportDecl () (HS.ModuleName () "Data.XDR.Types") True False False Nothing (Just xdrModule) Nothing
  , HS.ImportDecl () (HS.ModuleName () "Data.XDR.Serial") False False False Nothing Nothing Nothing
  , HS.ImportDecl () (HS.ModuleName () "Data.XDR.Specification") True False False Nothing (Just xdrModule) Nothing
  ]
  $ concatMap (definition s) l
