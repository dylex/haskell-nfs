-- | XDR Parser

{-# LANGUAGE TupleSections #-}
module Data.XDR.Parse
  ( parseFile
  ) where

import           Control.Applicative ((<|>))
import           Control.Arrow ((&&&))
import           Control.Monad (void)
import qualified Data.ByteString.Lazy.Char8 as BSLC
import           Data.Char (digitToInt)
import           Data.Functor.Identity (Identity)
import qualified Data.Map.Lazy as MapL
import qualified Data.Map.Strict as MapS
import           Data.Maybe (catMaybes, mapMaybe)
import qualified Data.Set as Set
import qualified Text.Parsec as P
import qualified Text.Parsec.Token as PT

import qualified Data.XDR.Types as XDR
import           Data.XDR.Specification hiding (arrayLength)

type Scope = MapL.Map Identifier Definition

baseScope :: Scope
baseScope = MapL.fromList $ (definitionIdentifier &&& id) <$> baseSpecification

type Stream = BSLC.ByteString
type Parser = P.Parsec Stream Scope

token :: PT.GenTokenParser Stream a Identity
token = PT.makeTokenParser PT.LanguageDef
  { PT.commentStart    = "/*"
  , PT.commentEnd      = "*/"
  , PT.commentLine     = "%"
  , PT.nestedComments  = False
  , PT.identStart      = P.letter
  , PT.identLetter     = P.alphaNum <|> P.char '_'
  , PT.opStart         = error "token op"
  , PT.opLetter        = error "token op"
  , PT.reservedNames   =
    [ "bool"
    , "case"
    , "const"
    , "default"
    , "double"
    , "quadruple"
    , "enum"
    , "float"
    , "hyper"
    , "int"
    , "opaque"
    , "string"
    , "struct"
    , "switch"
    , "typedef"
    , "union"
    , "unsigned"
    , "void"
    ]
  , PT.reservedOpNames = []
  , PT.caseSensitive   = True
  }

reserved :: String -> Parser ()
reserved = PT.reserved token

identifier :: Parser Identifier
identifier = PT.identifier token

endSemi1 :: Parser a -> Parser [a]
endSemi1 p = p `P.endBy1` PT.semi token

arrayLength, variableArrayLength :: Parser ArrayLength
variableArrayLength =
  VariableArray <$> PT.angles token (P.option maxLength value)
arrayLength =
  FixedArray    <$> PT.brackets token value
  <|> variableArrayLength

declaration :: Parser Declaration
declaration =
      typeDeclaration
  <|> opaqueDeclaration
  <|> stringDeclaration
  where
  typeDeclaration = do
    t <- typeSpecifier
    Declaration
        <$> (PT.symbol token "*" *> identifier)
        <*> pure (TypeOptional t)
      <|> Declaration
        <$> identifier
        <*> (TypeArray t <$> arrayLength <|> return (TypeSingle t))
  opaqueDeclaration =
    Declaration
      <$> (reserved "opaque" *> identifier)
      <*> (TypeOpaque <$> arrayLength)
  stringDeclaration =
    Declaration
      <$> (reserved "string" *> identifier)
      <*> (TypeString <$> variableArrayLength)

voidableDeclaration :: Parser OptionalDeclaration
voidableDeclaration =
      Just <$> declaration
  <|> Nothing <$ reserved "void"

constant :: Parser Integer
constant = (PT.lexeme token $
  nat <|> P.char '-' *> (negate <$> dec))
    P.<?> "constant" where
  nat = P.char '0' *> (P.oneOf "xX" *> number 16 P.hexDigit <|> number 8 P.octDigit <|> return 0) <|> dec
  dec = number 10 P.digit
  number base digit = foldl (\x d -> base*x + toInteger (digitToInt d)) 0 <$> P.many1 digit

valueFrom :: Integral n => MapS.Map Identifier n -> Parser n
valueFrom m = (fi =<< constant) <|> do
  v <- identifier
  maybe (do
    s <- P.getState
    case MapL.lookup v s of
      Just (Constant _ n) -> fi n
      _ -> fail $ "undefined constant: " ++ show v)
    return $ MapS.lookup v m
  where
  fi n
    | n == toInteger n' = return n'
    | otherwise = fail "invalid constant"
    where n' = fromInteger n

value :: Integral n => Parser n
value = valueFrom MapS.empty

typeSpecifier :: Parser TypeSpecifier
typeSpecifier = P.choice
  [ TypeInt        <$ reserved "int"
  , TypeHyper      <$ reserved "hyper"
  , reserved "unsigned" *> (
        TypeUnsignedInt   <$ reserved "int"
    <|> TypeUnsignedHyper <$ reserved "hyper"
    <|> return TypeUnsignedInt)
  , TypeFloat      <$ reserved "float"
  , TypeDouble     <$ reserved "double"
  , TypeQuadruple  <$ reserved "quadruple"
  , TypeBool       <$ reserved "bool"
  , reserved "enum"  *> (TypeEnum   <$> enumBody   <|> typeIdentifier)
  , reserved "struct"*> (TypeStruct <$> structBody <|> typeIdentifier)
  , reserved "union" *> (TypeUnion  <$> unionBody  <|> typeIdentifier)
  , typeIdentifier
  ] where
  typeIdentifier = TypeIdentifier <$> identifier

checkUnique :: (Ord k, Show k) => String -> [k] -> Parser (Set.Set k)
checkUnique t = ui Set.empty where
  ui m [] = return m
  ui m (k:l)
    | Set.member k m = fail $ "duplicate " ++ t ++ ": " ++ show k
    | otherwise = ui (Set.insert k m) l

enumBody :: Parser EnumBody
enumBody = do
  l <- PT.braces token $ PT.commaSep1 token $
    (,) <$> identifier <*> (PT.symbol token "=" *> value)
  _ <- checkUnique "enum identifier" $ fst <$> l
  _ <- checkUnique "enum value" $ snd <$> l
  return $ EnumBody l

structBody :: Parser StructBody
structBody = do
  l <- PT.braces token $ catMaybes <$> endSemi1 voidableDeclaration
  _ <- checkUnique "struct member" $ declarationIdentifier <$> l
  return $ StructBody l

unionBody :: Parser UnionBody
unionBody = do
  reserved "switch"
  d <- PT.parens token declaration
  r <- resolveTypeDescriptor $ declarationType d
  p <- case r of
    TypeSingle TypeInt -> return $ toInteger <$> (value :: Parser XDR.Int)
    TypeSingle TypeUnsignedInt -> return $ toInteger <$> (value :: Parser XDR.UnsignedInt)
    TypeSingle (TypeEnum (EnumBody v)) -> return $ toInteger <$> valueFrom (MapS.fromList v)
    _ -> fail "invalid discriminant declaration"
  PT.braces token $ do
    l <- endSemi1 ((,)
      <$> P.many1 (reserved "case" *> p <* PT.colon token)
      <*> voidableDeclaration)
    _ <- checkUnique "union member" $ mapMaybe (fmap declarationIdentifier . snd) l
    _ <- checkUnique "union case" $ fst =<< l
    f <- P.optionMaybe $ reserved "default" *> PT.colon token *> voidableDeclaration <* PT.semi token
    return $ UnionBody d [ (c, b) | (cs, b) <- l, c <- cs ] f

-- |Expand 'TypeSingle' 'TypeIdentifier'
resolveTypeDescriptor :: TypeDescriptor -> Parser TypeDescriptor
resolveTypeDescriptor (TypeSingle TypeBool) = resolveTypeDescriptor (TypeSingle (TypeIdentifier "bool"))
resolveTypeDescriptor (TypeSingle (TypeIdentifier i)) = do
  s <- P.getState
  case MapL.lookup i s of
    Just (TypeDef _ t) -> resolveTypeDescriptor t
    _ -> fail $ "undefined type: " ++ show i
resolveTypeDescriptor d = return d

def :: Parser Definition
def = constantDef <|> typeDef where
  constantDef = Constant
    <$> (reserved "const" *> identifier)
    <*> (PT.symbol token "=" *> constant)
  typeDef =
        reserved "typedef" *> (declDef <$> declaration)
    <|> TypeDef <$> (reserved "enum"   *> identifier) <*> (TypeSingle . TypeEnum   <$> enumBody)
    <|> TypeDef <$> (reserved "struct" *> identifier) <*> (TypeSingle . TypeStruct <$> structBody)
    <|> TypeDef <$> (reserved "union"  *> identifier) <*> (TypeSingle . TypeUnion  <$> unionBody)
  declDef (Declaration i t) = TypeDef i t

definition :: Parser Definition
definition = do
  d <- def
  case d of
    TypeDef _ t -> void $ resolveTypeDescriptor t
    _ -> return ()
  s <- P.getState
  case MapL.insertLookupWithKey (\_ -> const) (definitionIdentifier d) d s of
    (Nothing, s') -> P.putState s'
    _ -> fail $ "duplicate identifier: " ++ show (definitionIdentifier d)
  return d

specification :: Parser Specification
specification = endSemi1 definition

file :: Parser Specification
file = PT.whiteSpace token *> specification <* P.eof

parseFile :: FilePath -> IO (Either P.ParseError Specification)
parseFile f = P.runParser file baseScope f <$> BSLC.readFile f
