
{-# LANGUAGE ViewPatterns #-}
module Data.XDR.Generate
  ( generate
  ) where

import           Control.Arrow ((***), (&&&))
import           Data.Char (toLower, toUpper)
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe, maybeToList)
import qualified Data.Set as Set
import qualified Language.Haskell.Exts.Build as HS
import qualified Language.Haskell.Exts.Syntax as HS

import           Data.XDR.Specification
import qualified Data.XDR.Parse as XDR

-- |Set of non-unique identifiers, to which we will append a tick
type Scope = Set.Set Identifier

makeScope :: XDR.Scope -> Scope
makeScope = Map.keysSet . Map.filter XDR.bindingInitCaseConflict

mapHead :: (a -> a) -> [a] -> [a]
mapHead f (~(h:t)) = f h : t

identifier :: Scope -> (Char -> Char) -> Identifier -> String
identifier scope u i =
  (if Set.member i scope then (++"'") else id)
  $ mapHead u $ identifierString i

-- |Struct and union members
member :: (Char -> Char) -> String -> String -> String
member u s n = mapHead u s ++ '\'' : n

memberIdentifier :: (Char -> Char) -> String -> Identifier -> String
memberIdentifier u s = member u s . identifierString

unQual :: String -> HS.QName ()
unQual = HS.UnQual () . HS.name

xdrName :: String -> HS.QName ()
xdrName = HS.Qual () (HS.ModuleName () "XDR") . HS.name

instDecl :: String -> String -> [HS.Decl ()] -> HS.Decl ()
instDecl c t = HS.InstDecl () Nothing (HS.IRule () Nothing Nothing $ HS.IHApp () (HS.IHCon () $ unQual c) $ HS.TyCon () $ unQual t) . Just . map (HS.InsDecl ())

dataDecl :: String -> [HS.ConDecl ()] -> HS.Decl ()
dataDecl n c = HS.DataDecl () (HS.DataType ()) Nothing (HS.DHead () $ HS.name n)
  (map (HS.QualConDecl () Nothing Nothing) c)
  (Just $ HS.Deriving () [HS.IRule () Nothing Nothing (HS.IHCon () $ unQual "Show")])

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

specType :: Scope -> TypeSpecifier -> Maybe (HS.Type ())
specType scope (TypeIdentifier t) = Just $ HS.TyCon () $ unQual t' where
  t' = identifier scope toUpper t
specType _ t = HS.TyCon () . xdrName <$> primType t

lengthType :: String -> Length -> HS.Type ()
lengthType t l = HS.TyApp () (HS.TyCon () $ xdrName t) $ HS.TyPromoted () $ HS.PromotedInteger () (toInteger l) (show l)

descrType :: Scope -> TypeDescriptor -> Maybe (HS.Type ())
descrType scope (TypeSingle t) = specType scope t
descrType scope (TypeArray t (FixedLength l))    = HS.TyApp () (lengthType "FixedArray"  l) <$> specType scope t
descrType scope (TypeArray t (VariableLength l)) = HS.TyApp () (lengthType "Array"       l) <$> specType scope t
descrType _     (TypeOpaque  (FixedLength l))    = Just $       lengthType "FixedOpaque" l
descrType _     (TypeOpaque  (VariableLength l)) = Just $       lengthType "Opaque"      l
descrType _     (TypeString  (FixedLength l))    = Just $       lengthType "FixedString" l
descrType _     (TypeString  (VariableLength l)) = Just $       lengthType "String"      l
descrType scope (TypeOptional t) = HS.TyApp ()      (HS.TyCon () $ xdrName "Optional")      <$> specType scope t

declType' :: Scope -> Declaration -> HS.Type ()
declType' scope (Declaration n t) = fromMaybe (error $ "nested data structures are not supported: " ++ show n) $ descrType scope t

declaration :: Scope -> String -> Declaration -> HS.FieldDecl ()
declaration scope n d@(Declaration i _) =
  HS.FieldDecl () [HS.name $ memberIdentifier toLower n i] $ declType' scope d

optionalDeclaration :: Scope -> String -> OptionalDeclaration -> [HS.FieldDecl ()]
optionalDeclaration scope n = maybeToList . fmap (declaration scope n)

putFields :: HS.Exp () -> [HS.FieldDecl ()] -> HS.Exp ()
putFields _ [] = HS.app (HS.var $ HS.name "return") (HS.Con () $ HS.Special () $ HS.UnitCon ())
putFields x l = foldl1 (flip HS.infixApp $ HS.op $ HS.sym ">>")
  $ map (HS.app (HS.var $ HS.name "xdrPut") . flip HS.app x . HS.var)
  $ concatMap (\(HS.FieldDecl _ nl _) -> nl)
  $ l

definition :: Scope -> Definition -> [HS.Decl ()]
definition scope (Definition n (TypeDef (TypeSingle (TypeEnum (EnumBody el))))) =
  [ dataDecl hn $ map (flip (HS.ConDecl ()) [] . HS.name . fst) hel
  , instDecl "Enum" hn
    [ HS.FunBind () $ map (\(i,v) ->
        HS.Match () (HS.name "fromEnum") [HS.pApp (HS.name i) []] (HS.UnGuardedRhs () $ HS.intE v) Nothing)
      hel
    , HS.nameBind (HS.name "toEnum") (HS.var $ HS.name "xdrToEnum")
    ]
  , instDecl "XDR" hn
    [ HS.nameBind (HS.name "xdrPut") (HS.var $ HS.name "xdrPutEnum")
    , HS.nameBind (HS.name "xdrGet") (HS.var $ HS.name "xdrGetEnum")
    ]
  , instDecl "XDREnum" hn
    [ HS.FunBind () $ map (\(i,v) ->
        HS.Match () (HS.name "toXDREnum") [HS.intP v] (HS.UnGuardedRhs () $ HS.app (HS.Con () $ unQual "Just") $ HS.Con () $ unQual i) Nothing)
      hel ++
      [ HS.Match () (HS.name "toXDREnum") [HS.PWildCard ()] (HS.UnGuardedRhs () $ HS.Con () $ unQual "Nothing") Nothing]
    ]
  ] where
  hn = identifier scope toUpper n
  hel = map (identifier scope toUpper *** toInteger) el
definition scope (Definition n (TypeDef (TypeSingle (TypeStruct (StructBody dl))))) =
  [ dataDecl hn [HS.RecDecl () (HS.name hn) hdl]
  , instDecl "XDR" hn
    [ HS.simpleFun (HS.name "xdrPut") x $ putFields (HS.var x) hdl
    ]
  ] where
  x = HS.name "x"
  hn = identifier scope toUpper n
  hdl = map (declaration scope hn) dl
definition scope (Definition n (TypeDef (TypeSingle (TypeUnion (UnionBody d al o))))) =
  [ dataDecl hn $ map (\((_,l), b) ->
      HS.RecDecl () (HS.name l) b) hal
    ++ maybeToList (HS.RecDecl () (HS.name $ member toUpper hn "default") <$> ho)
  ] where
  hn = identifier scope toUpper n
  hd = declaration scope hn d
  hal = map ((unionCase &&& member toUpper hn . unionCaseLiteral) &&& optionalDeclaration scope hn . unionDeclaration) al
  ho = optionalDeclaration scope hn <$> o
definition scope (Definition n (TypeDef t)) =
  [ HS.TypeDecl () (HS.DHead () $ HS.name hn) $ declType' scope (Declaration n t)
  ] where
  hn = identifier scope toUpper n
definition scope (Definition n (Constant v)) =
  [ HS.TypeSig () [HS.name hn] constantType
  , HS.nameBind (HS.name hn) (HS.intE v)
  ] where
  hn = identifier scope toLower n

generate :: XDR.Scope -> String -> Specification -> HS.Module ()
generate s n l = HS.Module ()
  (Just $ HS.ModuleHead () (HS.ModuleName () n) Nothing Nothing)
  [ HS.LanguagePragma () $ map HS.name ["DataKinds"] ]
  [ HS.ImportDecl () (HS.ModuleName () "Prelude") False False False Nothing Nothing
    $ Just $ HS.ImportSpecList () False
    $ map (HS.IThingAll () . HS.name) ["Enum", "Maybe"]
    ++ map (HS.IVar () . HS.name) ["Integral", "Show", "return"]
    ++ [HS.IVar () $ HS.sym ">>"]
  , HS.ImportDecl () (HS.ModuleName () "Data.XDR.Types") True False False Nothing (Just $ HS.ModuleName () "XDR") Nothing
  , HS.ImportDecl () (HS.ModuleName () "Data.XDR.Serial") False False False Nothing Nothing Nothing
  , HS.ImportDecl () (HS.ModuleName () "Data.XDR.Specification") True False False Nothing (Just $ HS.ModuleName () "XDR") Nothing
  ]
  $ concatMap (definition $ makeScope s) l
