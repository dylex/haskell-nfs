
{-# LANGUAGE ViewPatterns #-}
module Data.XDR.Generate
  ( generateFromFile
  ) where

import           Control.Arrow ((***), (&&&))
import           Data.Char (toLower, toUpper, isAlpha, isUpper)
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe, maybeToList)
import qualified Data.Set as Set
import qualified Language.Haskell.Exts.Build as HS
import           Language.Haskell.Exts.Pretty (prettyPrintWithMode, PPHsMode(..), defaultMode)
import qualified Language.Haskell.Exts.Syntax as HS

import qualified Data.XDR as XDR
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

name :: String -> HS.Name ()
name s@(~(c:_))
  | isAlpha c || c == '_' = HS.Ident () s
  | otherwise = HS.Symbol () s

infix 9 !, !.
(!) :: String -> String -> HS.QName ()
(!) "" = HS.UnQual () . name
(!) m = HS.Qual () (HS.ModuleName () m) . name

(!.) :: String -> String -> HS.Exp ()
(!.) m n@(~(c:_))
  | isUpper c || c == ':' = HS.Con () $ m ! n
  | otherwise = HS.Var () $ m ! n

instDecl :: HS.QName () -> String -> [HS.Decl ()] -> HS.Decl ()
instDecl c t = HS.InstDecl () Nothing
  (HS.IRule () Nothing Nothing $ HS.IHApp () (HS.IHCon () c) $ HS.TyCon () $ ""!t)
  . Just . map (HS.InsDecl ())

dataDecl :: String -> [HS.ConDecl ()] -> [String] -> HS.Decl ()
dataDecl n con derive = HS.DataDecl () (HS.DataType ()) Nothing (HS.DHead () $ HS.name n)
  (map (HS.QualConDecl () Nothing Nothing) con)
  (Just $ HS.Deriving () $ map (HS.IRule () Nothing Nothing . HS.IHCon () . ("Prelude"!)) derive)

constantType :: HS.Type ()
constantType = HS.TyForall ()
  Nothing
  (Just $ HS.CxSingle () $ HS.ClassA () ("Prelude"!"Integral") [t])
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
specType scope (TypeIdentifier t) = Just $ HS.TyCon () $ ""!t' where
  t' = identifier scope toUpper t
specType _ t = HS.TyCon () . (!) "XDR" <$> primType t

specType' :: Scope -> TypeSpecifier -> HS.Type ()
specType' scope = fromMaybe (error $ "parameter data structures are not supported") . specType scope

lengthType :: String -> XDR.Length -> HS.Type ()
lengthType t l = HS.TyApp () (HS.TyCon () $ "XDR"!t) $ HS.TyPromoted () $ HS.PromotedInteger () (toInteger l) (show l)

descrType :: Scope -> TypeDescriptor -> Maybe (HS.Type ())
descrType scope (TypeSingle t) = specType scope t
descrType scope (TypeArray t (FixedLength l))    = HS.TyApp () (lengthType "FixedArray"  l) <$> specType scope t
descrType scope (TypeArray t (VariableLength l)) = HS.TyApp () (lengthType "Array"       l) <$> specType scope t
descrType _     (TypeOpaque  (FixedLength l))    = Just $       lengthType "FixedOpaque" l
descrType _     (TypeOpaque  (VariableLength l)) = Just $       lengthType "Opaque"      l
descrType _     (TypeString  (FixedLength l))    = Just $       lengthType "FixedString" l
descrType _     (TypeString  (VariableLength l)) = Just $       lengthType "String"      l
descrType scope (TypeOptional t) = HS.TyApp ()        (HS.TyCon () $ "XDR"!"Optional")      <$> specType scope t

declType' :: Scope -> Declaration -> HS.Type ()
declType' scope (Declaration n t) = fromMaybe (error $ "nested data structures are not supported: " ++ show n) $ descrType scope t

strictType :: HS.Type () -> HS.Type ()
strictType = HS.TyBang () (HS.BangedTy ()) (HS.NoUnpackPragma ())

declaration :: Scope -> String -> Declaration -> [HS.FieldDecl ()]
declaration scope n (Declaration i (TypeSingle (TypeStruct (StructBody dl)))) =
  concatMap (declaration scope $ memberIdentifier id n i) dl
declaration scope n d@(Declaration i _) =
  [HS.FieldDecl () [HS.name $ memberIdentifier toLower n i] $ strictType $ declType' scope d]

optionalDeclaration :: Scope -> String -> OptionalDeclaration -> [HS.FieldDecl ()]
optionalDeclaration scope = foldMap . declaration scope

typeDef :: Identifier -> HS.Decl ()
typeDef = HS.simpleFun (HS.name "xdrType") (HS.name "_") . HS.strE . identifierString

fieldNames :: [HS.FieldDecl ()] -> [HS.Name ()]
fieldNames = concatMap $ \(HS.FieldDecl _ nl _) -> nl

putFields :: HS.Exp () -> [HS.FieldDecl ()] -> HS.Exp ()
putFields _ [] = HS.app ("Control.Applicative"!."pure") (HS.Con () $ HS.Special () $ HS.UnitCon ())
putFields x l = foldl1 (flip HS.infixApp $ HS.QVarOp () $ "Control.Applicative"!"*>")
  $ map (HS.app ("XDR"!."xdrPut") . flip HS.app x . HS.var)
  $ fieldNames l

getFields :: HS.Exp () -> [HS.FieldDecl ()] -> HS.Exp ()
getFields n = foldl (\c _ -> HS.infixApp c (HS.QVarOp () $ "Control.Applicative"!"<*>") $ "XDR"!."xdrGet") n . fieldNames

pureCon :: String -> HS.Exp ()
pureCon = HS.app ("Control.Applicative"!."pure") . HS.Con () . (""!)

defaultIdentifier :: Identifier
defaultIdentifier = Identifier "default"

sMatch :: String -> HS.Pat () -> HS.Exp () -> HS.Match ()
sMatch n p e = HS.Match () (HS.name n) [p] (HS.UnGuardedRhs () e) Nothing

definition :: Scope -> Definition -> [HS.Decl ()]
definition scope (Definition n (TypeDef (TypeSingle (TypeEnum (EnumBody el))))) =
  [ dataDecl hn
    (map (flip (HS.ConDecl ()) [] . HS.name . fst) hel)
    ["Eq", "Ord", "Enum", "Bounded", "Show"]
  , instDecl ("XDR"!"XDR") hn
    [ typeDef n
    , HS.nameBind (HS.name "xdrPut") $ "XDR"!."xdrPutEnum"
    , HS.nameBind (HS.name "xdrGet") $ "XDR"!."xdrGetEnum"
    ]
  , instDecl ("XDR"!"XDREnum") hn
    [ HS.FunBind () $ map (\(i,v) ->
        sMatch "xdrFromEnum" (HS.pApp (HS.name i) []) $ HS.intE v)
      hel
    , HS.FunBind () $ map (\(i,v) ->
        sMatch "xdrToEnum" (HS.intP v) $ HS.app ("Prelude"!."return") $ HS.Con () $ ""!i)
      hel ++
      [ sMatch "xdrToEnum" (HS.PWildCard ()) $ HS.app ("Prelude"!."fail") $ HS.strE $ "invalid " ++ identifierString n]
    ]
  ] where
  hn = identifier scope toUpper n
  hel = map (identifier scope toUpper *** toInteger) el
definition scope (Definition n (TypeDef (TypeSingle (TypeStruct (StructBody dl))))) =
  [ dataDecl hn
    [HS.RecDecl () (HS.name hn) hdl]
    ["Eq", "Show"]
  , instDecl ("XDR"!"XDR") hn
    [ typeDef n
    , HS.simpleFun (HS.name "xdrPut") (HS.name "_x") $ putFields (HS.var $ HS.name "_x") hdl
    , HS.nameBind (HS.name "xdrGet") $ getFields (pureCon hn) hdl
    ]
  ] where
  hn = identifier scope toUpper n
  hdl = concatMap (declaration scope hn) dl
definition scope (Definition n (TypeDef (TypeSingle (TypeUnion (UnionBody d al o))))) =
  [ dataDecl hn
    (map (\((_,l),b) ->
      HS.RecDecl () (HS.name l) b) hal
    ++ maybeToList (HS.RecDecl () (HS.name hoc)
      . (HS.FieldDecl () [HS.name hom] (strictType hdt) :)
      <$> ho))
    ["Eq", "Show"]
  , HS.TypeSig () [HS.name hdn] $ HS.TyFun () (HS.TyCon () $ ""!hn) hdt
  , HS.nameBind (HS.name hdn) $ HS.infixApp ("XDR"!."xdrToEnum'") (HS.QVarOp () $ "Prelude"!".") $ "XDR"!."xdrDiscriminant"
  , instDecl ("XDR"!"XDR") hn
    [ typeDef n
    , HS.nameBind (HS.name "xdrPut") $ "XDR"!."xdrPutUnion"
    , HS.nameBind (HS.name "xdrGet") $ "XDR"!."xdrGetUnion"
    ]
  , instDecl ("XDR"!"XDRUnion") hn
    [ HS.FunBind () $ map (\((c,l),_) ->
        sMatch "xdrDiscriminant"
          (HS.PRec () (""!l) [])
          $ HS.intE c)
      hal
      ++ maybeToList ((
        sMatch "xdrDiscriminant" (HS.PRec () (""!hoc)
          [HS.PFieldPat () (""!hom) (HS.pvar $ HS.name "x")])
          $ HS.app ("XDR"!."xdrFromEnum") $ HS.var $ HS.name "x")
        <$ o)
    , HS.FunBind () $ map (\((_,l),b) ->
        sMatch "xdrPutUnionArm"
          (HS.PAsPat () (HS.name "_x") $ HS.PRec () (""!l) [])
          $ putFields (HS.var $ HS.name "_x") b)
      hal
      ++ maybeToList ((sMatch "xdrPutUnionArm"
          (HS.PAsPat () (HS.name "_x") $ HS.PRec () (""!hoc) [])
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
            (HS.app ("Prelude"!."fail") $ HS.strE $ "invalid " ++ identifierString n ++ " discriminant")
            (getFields (HS.infixApp (HS.Con () $ ""!hoc) (HS.QVarOp () $ "Control.Applicative"!"<$>")
              (HS.app ("XDR"!."xdrToEnum") $ HS.var $ HS.name "_c")))
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
  , HS.nameBind (HS.name hn) $ HS.intE v
  ] where
  hn = identifier scope toLower n
definition scope (Definition n (Program vl px)) =
  [ HS.TypeSig () [HS.name hn'] $ HS.TyCon () $ ""!hn
  , HS.nameBind (HS.name hn') $ HS.appFun (""!.hn) $ map (\(vn, Version _ rl vx) ->
      HS.appFun (""!.vn) $ map (\(Procedure _ _ _ rx) ->
          HS.appFun ("RPC"!."Procedure") $ map (HS.intE . toInteger) [px, vx, rx])
        rl)
      hvl
  , dataDecl hn [HS.RecDecl () (HS.name hn) (map (\(vn, _) ->
      HS.FieldDecl () [HS.name $ mapHead toLower vn] $ strictType $ HS.TyCon () $ ""!vn)
    hvl)] []
  ] ++ map (\(vn, Version _ rl _) ->
    dataDecl vn [HS.RecDecl () (HS.name vn) (map (\(Procedure rr rn ra _) ->
      HS.FieldDecl () [HS.name $ memberIdentifier toLower vn rn]
        $ strictType $ HS.TyApp () (HS.TyApp () (HS.TyCon () $ "RPC"!"Procedure")
        $ tt $ map (specType' scope) ra)
        $ maybe (HS.unit_tycon ()) (specType' scope) rr)
    rl)] []
  ) hvl
  where
  hn = identifier scope toUpper n
  hn' = mapHead toLower hn
  hvl = map (memberIdentifier toUpper hn . versionIdentifier &&& id) vl
  tt [] = HS.unit_tycon ()
  tt [t] = t
  tt l = HS.TyTuple () HS.Boxed l

hasProgramDefinition :: Specification -> Bool
hasProgramDefinition = any isProgramDefinition where
  isProgramDefinition (Definition _ (Program _ _)) = True
  isProgramDefinition _ = False

generate :: XDR.Scope -> String -> Specification -> HS.Module ()
generate s n l = HS.Module ()
  (Just $ HS.ModuleHead () (HS.ModuleName () n) Nothing Nothing)
  [ HS.LanguagePragma () $ map HS.name ["DataKinds", "MultiParamTypeClasses", "TypeSynonymInstances"] ]
  ([HS.ImportDecl () (HS.ModuleName () "Prelude") True False False Nothing Nothing Nothing
  , HS.ImportDecl () (HS.ModuleName () "Control.Applicative") True False False Nothing Nothing Nothing
  , HS.ImportDecl () (HS.ModuleName () "Data.XDR") True False False Nothing (Just $ HS.ModuleName () "XDR") Nothing
  ] ++ if hasProgramDefinition l then
  [ HS.ImportDecl () (HS.ModuleName () "Network.ONCRPC.Types") True False False Nothing (Just $ HS.ModuleName () "RPC") Nothing ]
  else [])
  $ concatMap (definition $ makeScope s) l

generateFromFile :: FilePath -> String -> IO String
generateFromFile f m = do
  (d, s) <- either (fail . show) return =<< XDR.parseFile f
  let h = generate s m d
  return $ "-- Generated from " ++ f ++ " by oncrpc/hsxdrgen\n"
    ++ prettyPrintWithMode defaultMode
      { classIndent = 2
      , doIndent = 2
      , multiIfIndent = 2
      , caseIndent = 2
      , letIndent = 2
      , whereIndent = 2
      , onsideIndent = 2
      } h
