-- |Generate Haskell code from XDR descriptions as per RFC4506 and RPC extensions from RFC5531

{-# LANGUAGE RecordWildCards #-}
module Network.ONCRPC.XDR.Generate
  ( generateFromFile
  , generate
  , generateModule
  , ReidentOptions(..)
  , GenerateOptions(..)
  , defaultReidentOptions
  ) where

import           Control.Arrow ((***), (&&&))
import qualified Data.ByteString.Lazy.Char8 as BSLC
import           Data.Char (isAlpha, isUpper)
import           Data.Maybe (fromMaybe, maybeToList)
import qualified Language.Haskell.Exts.Build as HS
import           Language.Haskell.Exts.Pretty (prettyPrintWithMode, PPHsMode(..), defaultMode)
import qualified Language.Haskell.Exts.Syntax as HS

import qualified Network.ONCRPC.XDR as XDR
import           Network.ONCRPC.XDR.Specification
import qualified Network.ONCRPC.XDR.Parse as XDR
import           Network.ONCRPC.XDR.Reident

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

instDecl :: HS.QName () -> String -> [HS.InstDecl ()] -> HS.Decl ()
instDecl c t = HS.InstDecl () Nothing
  (HS.IRule () Nothing Nothing $ HS.IHApp () (HS.IHCon () c) $ HS.TyCon () $ ""!t)
  . Just

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

specType :: TypeSpecifier -> Maybe (HS.Type ())
specType (TypeIdentifier t) = Just $ HS.TyCon () $ ""!t
specType t = HS.TyCon () . (!) "XDR" <$> primType t

specType' :: TypeSpecifier -> HS.Type ()
specType' = fromMaybe (error $ "parameter data structures are not supported") . specType

lengthType :: String -> XDR.Length -> HS.Type ()
lengthType t l = HS.TyApp () (HS.TyCon () $ "XDR"!t) $ HS.TyPromoted () $ HS.PromotedInteger () (toInteger l) (show l)

descrType :: TypeDescriptor -> Maybe (HS.Type ())
descrType (TypeSingle t) = specType t
descrType (TypeArray t (FixedLength l))    = HS.TyApp () (lengthType "FixedArray"  l) <$> specType t
descrType (TypeArray t (VariableLength l)) = HS.TyApp () (lengthType "Array"       l) <$> specType t
descrType (TypeOpaque  (FixedLength l))    = Just $       lengthType "FixedOpaque" l
descrType (TypeOpaque  (VariableLength l)) = Just $       lengthType "Opaque"      l
descrType (TypeString  (FixedLength l))    = Just $       lengthType "FixedString" l
descrType (TypeString  (VariableLength l)) = Just $       lengthType "String"      l
descrType (TypeOptional t) = HS.TyApp ()        (HS.TyCon () $ "XDR"!"Optional")      <$> specType t

declType' :: Declaration -> HS.Type ()
declType' (Declaration n t) = fromMaybe (error $ "nested data structures are not supported: " ++ show n) $ descrType t

strictType :: HS.Type () -> HS.Type ()
strictType = HS.TyBang () (HS.BangedTy ()) (HS.NoUnpackPragma ())

declaration :: Declaration -> [HS.FieldDecl ()]
declaration (Declaration _ (TypeSingle (TypeStruct (StructBody dl)))) =
  concatMap declaration dl
declaration d@(Declaration i _) =
  [HS.FieldDecl () [HS.name i] $ strictType $ declType' d]

optionalDeclaration :: OptionalDeclaration -> [HS.FieldDecl ()]
optionalDeclaration = foldMap declaration

typeDef :: String -> HS.Decl ()
typeDef = HS.simpleFun (HS.name "xdrType") (HS.name "_") . HS.strE

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

sMatch :: String -> HS.Pat () -> HS.Exp () -> HS.Match ()
sMatch n p e = HS.Match () (HS.name n) [p] (HS.UnGuardedRhs () e) Nothing

definition :: Definition -> [HS.Decl ()]
definition (Definition n (TypeDef (TypeSingle (TypeEnum (EnumBody el))))) =
  [ dataDecl n
    (map (flip (HS.ConDecl ()) [] . HS.name . fst) el)
    ["Eq", "Ord", "Enum", "Bounded", "Show"]
  , instDecl ("XDR"!"XDR") n $ map (HS.InsDecl ())
    [ typeDef n
    , HS.nameBind (HS.name "xdrPut") $ "XDR"!."xdrPutEnum"
    , HS.nameBind (HS.name "xdrGet") $ "XDR"!."xdrGetEnum"
    ]
  , instDecl ("XDR"!"XDREnum") n $ map (HS.InsDecl ())
    [ HS.FunBind () $ map (\(i,v) ->
        sMatch "xdrFromEnum" (HS.pApp (HS.name i) []) $ HS.intE $ toInteger v)
      el
    , HS.FunBind () $ map (\(i,v) ->
        sMatch "xdrToEnum" (HS.intP $ toInteger v) $ HS.app ("Prelude"!."return") $ HS.Con () $ ""!i)
      el ++
      [ sMatch "xdrToEnum" (HS.PWildCard ()) $ HS.app ("Prelude"!."fail") $ HS.strE $ "invalid " ++ n]
    ]
  ]
definition (Definition n (TypeDef (TypeSingle (TypeStruct (StructBody dl))))) =
  [ dataDecl n
    [HS.RecDecl () (HS.name n) hdl]
    ["Eq", "Show"]
  , instDecl ("XDR"!"XDR") n $ map (HS.InsDecl ())
    [ typeDef n
    , HS.simpleFun (HS.name "xdrPut") (HS.name "_x") $ putFields (HS.var $ HS.name "_x") hdl
    , HS.nameBind (HS.name "xdrGet") $ getFields (pureCon n) hdl
    ]
  ] where
  hdl = concatMap declaration dl
definition (Definition n (TypeDef (TypeSingle (TypeUnion (UnionBody d@(Declaration dn _) cl o))))) =
  [ dataDecl n
    (map (\(_,(l,b)) ->
      HS.RecDecl () (HS.name l) b) hcl
    ++ maybe [] (\(l,b) -> [HS.RecDecl () (HS.name l)
      $ HS.FieldDecl () [HS.name hom] (strictType hdt) : b])
      ho)
    ["Eq", "Show"]
  , HS.TypeSig () [HS.name dn] $ HS.TyFun () (HS.TyCon () $ ""!n) hdt
  , HS.nameBind (HS.name dn) $ "XDR"!."xdrDiscriminant"
  , instDecl ("XDR"!"XDR") n $ map (HS.InsDecl ())
    [ typeDef n
    , HS.nameBind (HS.name "xdrPut") $ "XDR"!."xdrPutUnion"
    , HS.nameBind (HS.name "xdrGet") $ "XDR"!."xdrGetUnion"
    ]
  , instDecl ("XDR"!"XDRUnion") n
    [ HS.InsType () (HS.TyApp () (HS.TyCon () $ ""!"XDRDiscriminant") (HS.TyCon () $ ""!n)) hdt
    , HS.InsDecl () $ HS.FunBind () $ map
        (uncurry (split [] . HS.intE))
        hcl
      ++ maybeToList (split
          [HS.PFieldPat () (""!hom) (HS.pvar $ HS.name "d")]
          (HS.app ("XDR"!."xdrFromEnum") (""!."d"))
        <$> ho)
    , HS.InsDecl () $ HS.FunBind () $ map (\(c,(l,b)) ->
        sMatch "xdrGetUnionArm"
          (HS.intP c)
          $ getFields (pureCon l) b)
      hcl
      ++ [sMatch "xdrGetUnionArm"
          (HS.pvar $ HS.name "_c")
          $ maybe
            (HS.app ("Prelude"!."fail") $ HS.strE $ "invalid " ++ n ++ " discriminant")
            (\(l,b) -> getFields (HS.infixApp (HS.Con () $ ""!l) (HS.QVarOp () $ "Control.Applicative"!"<$>")
              (HS.app ("XDR"!."xdrToEnum") $ HS.var $ HS.name "_c")) b)
            ho]
    ]
  ] where
  split p c (l,b) = sMatch "xdrSplitUnion"
    (HS.PAsPat () (HS.name "_x") $ HS.PRec () (""!l) p)
    $ HS.tuple [c, putFields (""!."_x") b]
  hdt = declType' d
  hcl = map (toInteger *** arm) cl
  hom = dn ++ "'"
  ho = arm <$> o
  arm = unionCaseIdentifier &&& optionalDeclaration . unionDeclaration
definition (Definition n (TypeDef t)) =
  [ HS.TypeDecl () (HS.DHead () $ HS.name n) $ declType' (Declaration n t)
  ]
definition (Definition n (Constant v)) =
  [ HS.TypeSig () [HS.name n] constantType
  , HS.nameBind (HS.name n) $ HS.intE v
  ]
definition (Definition n (Program t vl px)) =
  [ HS.TypeSig () [HS.name n] $ HS.TyCon () $ ""!t
  , HS.nameBind (HS.name n) $ HS.appFun (""!.t) $ map (\(Version _ vt rl vx) ->
      HS.appFun (""!.vt) $ map (\(Procedure _ _ _ rx) ->
          HS.appFun ("RPC"!."Procedure") $ map (HS.intE . toInteger) [px, vx, rx])
        rl)
      vl
  , dataDecl t [HS.RecDecl () (HS.name t) (map (\(Version vn vt _ _) ->
      HS.FieldDecl () [HS.name vn] $ strictType $ HS.TyCon () $ ""!vt)
    vl)] []
  ] ++ map (\(Version _ vt rl _) ->
    dataDecl vt [HS.RecDecl () (HS.name vt) (map (\(Procedure rr rn ra _) ->
      HS.FieldDecl () [HS.name rn]
        $ strictType $ HS.TyApp () (HS.TyApp () (HS.TyCon () $ "RPC"!"Procedure")
        $ tt $ map specType' ra)
        $ maybe (HS.unit_tycon ()) specType' rr)
    rl)] []
  ) vl
  where
  tt [] = HS.unit_tycon ()
  tt [a] = a
  tt l = HS.TyTuple () HS.Boxed l

hasProgramDefinition :: Specification -> Bool
hasProgramDefinition = any isProgramDefinition where
  isProgramDefinition (Definition _ Program{}) = True
  isProgramDefinition _ = False

specification :: String -> Specification -> HS.Module ()
specification n l = HS.Module ()
  (Just $ HS.ModuleHead () (HS.ModuleName () n) Nothing Nothing)
  [ HS.LanguagePragma () $ map HS.name ["DataKinds", "TypeFamilies"] ]
  ([HS.ImportDecl () (HS.ModuleName () "Prelude") True False False Nothing Nothing Nothing
  , HS.ImportDecl () (HS.ModuleName () "Control.Applicative") True False False Nothing Nothing Nothing
  , HS.ImportDecl () (HS.ModuleName () "Network.ONCRPC.XDR") True False False Nothing (Just $ HS.ModuleName () "XDR") Nothing
  ] ++ if hasProgramDefinition l then
  [ HS.ImportDecl () (HS.ModuleName () "Network.ONCRPC.Types") True False False Nothing (Just $ HS.ModuleName () "RPC") Nothing ]
  else [])
  $ concatMap definition l

-- |Options for generating Haskell code
data GenerateOptions = GenerateOptions
  { generateModuleName :: String -- ^Name for the generated module
  , generateReidentOptions :: ReidentOptions
  }
  deriving (Eq, Show)

-- |Parse an XDR specification and generate a Haskell module, or fail on error.
-- The 'String' argument provides a description of the input to use in parse errors.
generateModule :: Monad m => GenerateOptions -> String -> BSLC.ByteString -> m (HS.Module ())
generateModule GenerateOptions{..} n b = do
  (d, s) <- either (fail . show) return $ XDR.parse n b
  return $ specification generateModuleName $ reident generateReidentOptions s d

-- |Parse an XDR specification and generate pretty-printed Haskell source string, or fail on error.
-- The 'String' argument provides a description of the input to use in parse errors.
generate :: Monad m => GenerateOptions -> String -> BSLC.ByteString -> m String
generate opts n s = do
  m <- generateModule opts n s
  return $ "-- |Generated from " ++ n ++ " by <https://github.com/dylex/oncrpc hsrpcgen>\n"
    ++ prettyPrintWithMode defaultMode
      { classIndent   = 2
      , doIndent      = 2
      , multiIfIndent = 2
      , caseIndent    = 2
      , letIndent     = 2
      , whereIndent   = 2
      , onsideIndent  = 2
      } m

-- |'generate' from a file.
generateFromFile :: GenerateOptions -> FilePath -> IO String
generateFromFile opts f = generate opts f =<< BSLC.readFile f
