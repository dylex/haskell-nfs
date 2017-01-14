{-# LANGUAGE ViewPatterns #-}
module Data.XDR.Generate
  ( generate
  ) where

import           Data.Char (toLower, toUpper)
import qualified Data.Map as Map
import qualified Language.Haskell.Exts.Syntax as HS

import qualified Data.XDR.Types as XDR
import           Data.XDR.Specification
import qualified Data.XDR.Parse as XDR

xdrModule :: HS.ModuleName ()
xdrModule = HS.ModuleName () "XDR"

identifierName :: XDR.Scope -> Bool -> Identifier -> HS.Name ()
identifierName s u i@(~(h:r)) = HS.Ident ()
  $ (if any XDR.bindingInitCaseConflict $ Map.lookup i s then (++"'") else id)
  $ (if u then toUpper else toLower) h : r

constantType :: HS.Type ()
constantType = HS.TyForall ()
  (Just [HS.UnkindedVar () v])
  (Just $ HS.CxSingle () $ HS.ClassA () (HS.UnQual () $ HS.Ident () "Integral") [t])
  t
  where
  t = HS.TyVar () v
  v = HS.Ident () "a"

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

definition :: XDR.Scope -> Definition -> [HS.Decl ()]
definition s (Definition i (Constant v)) =
  [ HS.TypeSig () [n] constantType
  , HS.PatBind () (HS.PVar () n) (HS.UnGuardedRhs () $ HS.Lit () $ HS.Int () v (show v)) Nothing
  ] where
  n = identifierName s False i
definition s (Definition i (TypeDef (TypeSingle (primType -> Just t)))) =
  [ HS.TypeDecl () (HS.DHead () n) $ HS.TyCon () $ HS.Qual () xdrModule $ HS.Ident () t
  ] where
  n = identifierName s True i
definition s (Definition i (TypeDef (TypeSingle (TypeIdentifier ti)))) =
  [ HS.TypeDecl () (HS.DHead () n) $ HS.TyCon () $ HS.UnQual () tn
  ] where
  n = identifierName s True i
  tn = identifierName s True ti
definition _ _ = []

generate :: String -> XDR.Scope -> Specification -> HS.Module ()
generate n s l = HS.Module ()
  (Just $ HS.ModuleHead () (HS.ModuleName () n) Nothing Nothing)
  []
  [ HS.ImportDecl () (HS.ModuleName () "Prelude") False False False Nothing Nothing (Just $ HS.ImportSpecList () False [])
  , HS.ImportDecl () (HS.ModuleName () "Data.XDR.Types") True False False Nothing (Just xdrModule) Nothing
  ]
  $ concatMap (definition s) l
