
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
instDecl c t = HS.InstDecl () Nothing
  (HS.IRule () Nothing Nothing $ HS.IHApp () (HS.IHCon () $ unQual c) $ HS.TyCon () $ unQual t)
  . Just . map (HS.InsDecl ())

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

declaration :: Scope -> String -> Declaration -> [HS.FieldDecl ()]
declaration scope n (Declaration i (TypeSingle (TypeStruct (StructBody dl)))) =
  concatMap (declaration scope $ memberIdentifier id n i) dl
declaration scope n d@(Declaration i _) =
  [HS.FieldDecl () [HS.name $ memberIdentifier toLower n i] $ declType' scope d]

optionalDeclaration :: Scope -> String -> OptionalDeclaration -> [HS.FieldDecl ()]
optionalDeclaration scope = foldMap . declaration scope

typeDef :: Identifier -> HS.Decl ()
typeDef = HS.simpleFun (HS.name "xdrType") (HS.name "_") . HS.strE . identifierString

fieldNames :: [HS.FieldDecl ()] -> [HS.Name ()]
fieldNames = concatMap $ \(HS.FieldDecl _ nl _) -> nl

putFields :: HS.Exp () -> [HS.FieldDecl ()] -> HS.Exp ()
putFields _ [] = HS.app (HS.var $ HS.name "pure") (HS.Con () $ HS.Special () $ HS.UnitCon ())
putFields x l = foldl1 (flip HS.infixApp $ HS.op $ HS.sym "*>")
  $ map (HS.app (HS.var $ HS.name "xdrPut") . flip HS.app x . HS.var)
  $ fieldNames l

getFields :: HS.Exp () -> [HS.FieldDecl ()] -> HS.Exp ()
getFields n = foldl (\c _ -> HS.infixApp c (HS.op $ HS.sym "<*>") $ HS.var $ HS.name "xdrGet") n . fieldNames

pureCon :: String -> HS.Exp ()
pureCon = HS.app (HS.var $ HS.name "pure") . HS.Con () . unQual

defaultIdentifier :: Identifier
defaultIdentifier = Identifier "default"

sMatch :: String -> HS.Pat () -> HS.Exp () -> HS.Match ()
sMatch n p e = HS.Match () (HS.name n) [p] (HS.UnGuardedRhs () e) Nothing

definition :: Scope -> Definition -> [HS.Decl ()]
definition scope (Definition n (TypeDef (TypeSingle (TypeEnum (EnumBody el))))) =
  [ dataDecl hn $ map (flip (HS.ConDecl ()) [] . HS.name . fst) hel
  , instDecl "Enum" hn
    [ HS.FunBind () $ map (\(i,v) ->
        sMatch "fromEnum" (HS.pApp (HS.name i) []) $ HS.intE v)
      hel
    , HS.nameBind (HS.name "toEnum") $ HS.var $ HS.name "xdrToEnum"
    ]
  , instDecl "XDR" hn
    [ typeDef n
    , HS.nameBind (HS.name "xdrPut") $ HS.var $ HS.name "xdrPutEnum"
    , HS.nameBind (HS.name "xdrGet") $ HS.var $ HS.name "xdrGetEnum"
    ]
  , instDecl "XDREnum" hn
    [ HS.FunBind () $ map (\(i,v) ->
        sMatch "toXDREnum" (HS.intP v) $ HS.app (HS.var $ HS.name "return") $ HS.Con () $ unQual i)
      hel ++
      [ sMatch "toXDREnum" (HS.PWildCard ()) $ HS.app (HS.var $ HS.name "fail") $ HS.strE $ "invalid " ++ identifierString n]
    ]
  ] where
  hn = identifier scope toUpper n
  hel = map (identifier scope toUpper *** toInteger) el
definition scope (Definition n (TypeDef (TypeSingle (TypeStruct (StructBody dl))))) =
  [ dataDecl hn [HS.RecDecl () (HS.name hn) hdl]
  , instDecl "XDR" hn
    [ typeDef n
    , HS.simpleFun (HS.name "xdrPut") (HS.name "_x") $ putFields (HS.var $ HS.name "_x") hdl
    , HS.nameBind (HS.name "xdrGet") $ getFields (pureCon hn) hdl
    ]
  ] where
  hn = identifier scope toUpper n
  hdl = concatMap (declaration scope hn) dl
definition scope (Definition n (TypeDef (TypeSingle (TypeUnion (UnionBody d al o))))) =
  [ dataDecl hn $ map (\((_,l),b) ->
      HS.RecDecl () (HS.name l) b) hal
    ++ maybeToList (HS.RecDecl () (HS.name hoc)
      . (HS.FieldDecl () [HS.name hom] hdt :)
      <$> ho)
  , HS.TypeSig () [HS.name hdn] $ HS.TyFun () (HS.TyCon () $ unQual hn) hdt
  , HS.nameBind (HS.name hdn) $ HS.infixApp (HS.var $ HS.name "toXDREnum'") (HS.op $ HS.sym ".") $ HS.var $ HS.name "xdrDiscriminant"
  , instDecl "XDR" hn
    [ typeDef n
    , HS.nameBind (HS.name "xdrPut") $ HS.var $ HS.name "xdrPutUnion"
    , HS.nameBind (HS.name "xdrGet") $ HS.var $ HS.name "xdrGetUnion"
    ]
  , instDecl "XDRUnion" hn
    [ HS.FunBind () $ map (\((c,l),_) ->
        sMatch "xdrDiscriminant"
          (HS.PRec () (unQual l) [])
          $ HS.intE c)
      hal
      ++ maybeToList ((
        sMatch "xdrDiscriminant" (HS.PRec () (unQual hoc)
          [HS.PFieldPat () (unQual hom) (HS.pvar $ HS.name "x")])
          $ HS.app (HS.var $ HS.name "fromXDREnum") $ HS.var $ HS.name "x")
        <$ o)
    , HS.FunBind () $ map (\((_,l),b) ->
        sMatch "xdrPutUnionArm"
          (HS.PAsPat () (HS.name "_x") $ HS.PRec () (unQual l) [])
          $ putFields (HS.var $ HS.name "_x") b)
      hal
      ++ maybeToList ((sMatch "xdrPutUnionArm"
          (HS.PAsPat () (HS.name "_x") $ HS.PRec () (unQual hoc) [])
          . putFields (HS.var $ HS.name "_x"))
        <$> ho)
    , HS.FunBind () $ map (\((c,l),b) ->
        sMatch "xdrGetUnionArm"
          (HS.intP c)
          $ getFields (pureCon l) b)
      hal
      ++ [sMatch "xdrGetUnionArm"
          (HS.pvar $ HS.name "_c")
          $ maybe
            (HS.app (HS.var $ HS.name "fail") $ HS.strE $ "invalid " ++ identifierString n ++ " discriminant")
            (getFields (HS.infixApp (HS.Con () $ unQual hoc) (HS.op $ HS.sym "<$>")
              (HS.app (HS.var $ HS.name "toXDREnum") $ HS.var $ HS.name "_c")))
            ho]
    ]
  ] where
  hn = identifier scope toUpper n
  hdn = memberIdentifier toLower hn $ declarationIdentifier d
  hdt = declType' scope d
  hal = map ((toInteger . unionCase &&& member toUpper hn . unionCaseLiteral) &&& optionalDeclaration scope hn . unionDeclaration) al
  hoc = memberIdentifier toUpper hn defaultIdentifier
  hom = mapHead toLower hoc
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
definition _ (Definition _ (Program _ _)) = [] -- TODO

generate :: XDR.Scope -> String -> Specification -> HS.Module ()
generate s n l = HS.Module ()
  (Just $ HS.ModuleHead () (HS.ModuleName () n) Nothing Nothing)
  [ HS.LanguagePragma () $ map HS.name ["DataKinds", "MultiParamTypeClasses", "TypeSynonymInstances"] ]
  [ HS.ImportDecl () (HS.ModuleName () "Prelude") False False False Nothing Nothing
    $ Just $ HS.ImportSpecList () False
    $ (HS.IThingAll () $ HS.name "Enum")
    : map (HS.IVar ()) [HS.name "Integral", HS.name "Show", HS.name "return", HS.name "fail", HS.sym "."]
  , HS.ImportDecl () (HS.ModuleName () "Control.Applicative") False False False Nothing Nothing
    $ Just $ HS.ImportSpecList () False
    $ map (HS.IVar ()) [HS.sym "*>", HS.sym "<*>", HS.sym "<$>", HS.name "pure"]
  , HS.ImportDecl () (HS.ModuleName () "Data.XDR.Types") True False False Nothing (Just $ HS.ModuleName () "XDR") Nothing
  , HS.ImportDecl () (HS.ModuleName () "Data.XDR.Serial") False False False Nothing Nothing Nothing
  ]
  $ concatMap (definition $ makeScope s) l
