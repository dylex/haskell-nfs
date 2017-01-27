-- |Convert XDR identifiers to Haskell identifiers.
-- Rules to convert identifiers in a 'Specification' to follow Haskell's lexical rules and ensure uniqueness for Haskell's scoping.
{-# LANGUAGE RecordWildCards #-}
module Network.ONCRPC.XDR.Reident
  ( ReidentOptions(..)
  , defaultReidentOptions
  , reident
  ) where

import           Control.Arrow (first, second)
import           Data.Char (isLower, isUpper, toLower, toUpper)
import qualified Data.Map as Map
import qualified Data.Set as Set

import           Network.ONCRPC.XDR.Specification
import qualified Network.ONCRPC.XDR.Parse as XDR

-- |How to generate Haskell identifiers from XDR in order to confirm to Haskell's lexical rules and ensure uniqueness.
data ReidentOptions = ReidentOptions
  { reidentUpperPrefix, reidentLowerPrefix :: String -- ^Prefix to use to make an identifier a different case if necessary, e.g. @\"_\"@ for lower-case, or @\"XDR_\"@ for upper case (default empty: just changes the first character, possibly resulting in names like @\"nFS_NULL\"@)
  , reidentJoinField, reidentJoinProcedure :: Maybe String -- ^Prefix fields with their type name (or program, version name) and this string (necessary for most XDR files), or @Nothing@ to use only the field name (or procedure name), which assumes uniqueness across the file (e.g., if you wrote the file yourself, though often safe for procedures only) (default @Just \"\'\"@)
  }
  deriving (Eq, Show)

defaultReidentOptions :: ReidentOptions
defaultReidentOptions = ReidentOptions
  { reidentUpperPrefix = ""
  , reidentLowerPrefix = ""
  , reidentJoinField = Just "'"
  , reidentJoinProcedure = Just "'"
  }

data ReidentOps = ReidentOps
  { reidentUpper, reidentLower :: String -> String
  , reidentField, reidentProcedure :: String -> String -> String
  , reidentUnique :: String -> String
  }

reidentOps :: ReidentOptions -> XDR.Scope -> ReidentOps
reidentOps ReidentOptions{..} scope = ReidentOps
  { reidentUpper = toUpperPrefix reidentUpperPrefix
  , reidentLower = toLowerPrefix reidentLowerPrefix
  , reidentField = joinField reidentJoinField
  , reidentProcedure = joinField reidentJoinProcedure
  , reidentUnique = unique
  } where
  toUpperPrefix p s@(~(h:t))
    | isUpper h = s
    | null p = toUpper h : t
    | otherwise = p ++ s
  toLowerPrefix p s@(~(h:t))
    | isLower h = s
    | null p = toLower h : t
    | otherwise = p ++ s
  joinField (Just c) p n = p ++ c ++ n
  joinField Nothing _ n = n
  unique n
    | Set.member n dups = n ++ "'"
    | otherwise = n
  dups = Map.keysSet $ Map.filter XDR.bindingInitCaseConflict scope

declaration :: ReidentOps -> String -> Declaration -> Declaration
declaration ops n (Declaration m t) = Declaration (reidentLower ops nm) (typeDescriptor ops nm t) where
  nm = reidentField ops n m

typeSpecifier :: ReidentOps -> String -> TypeSpecifier -> TypeSpecifier
typeSpecifier ops _ (TypeEnum (EnumBody el)) = TypeEnum $ 
  EnumBody $ map (first $ reidentUnique ops) el
typeSpecifier ops n (TypeStruct (StructBody dl)) = TypeStruct $ 
  StructBody $ map (declaration ops n) dl
typeSpecifier ops n (TypeUnion (UnionBody d cl o)) = TypeUnion $
  UnionBody (decl d) (map (second arm) cl) (arm <$> o) where
  arm (UnionArm l m) = UnionArm (con l) (decl <$> m)
  con l = reidentUpper ops $ n ++ '\'' : l
  decl = declaration ops n
typeSpecifier ops _ (TypeIdentifier i) = TypeIdentifier $
  reidentUpper ops $ reidentUnique ops i
typeSpecifier _ _ t = t

typeDescriptor :: ReidentOps -> String -> TypeDescriptor -> TypeDescriptor
typeDescriptor ops n (TypeSingle t) = TypeSingle (typeSpecifier ops n t)
typeDescriptor ops n (TypeArray t l) = TypeArray (typeSpecifier ops n t) l
typeDescriptor ops n (TypeOptional t) = TypeOptional (typeSpecifier ops n t)
typeDescriptor _ _ t = t

procedure :: ReidentOps -> String -> Procedure -> Procedure
procedure ops n (Procedure r m al x) = Procedure (ts <$> r) (reidentLower ops nm) (ts <$> al) x where
  nm = reidentProcedure ops n m
  ts = typeSpecifier ops nm

version :: ReidentOps -> String -> Version -> Version
version ops n (Version m t pl x) = Version (reidentLower ops nm) (reidentUpper ops nt) (map (procedure ops nm) pl) x where
  nm = reidentProcedure ops n m
  nt = reidentProcedure ops n t

makeDefinition :: ReidentOps -> String -> DefinitionBody -> Definition
makeDefinition ops n (TypeDef d) = Definition (reidentUpper ops n) $ TypeDef $ typeDescriptor ops n d
makeDefinition ops n (Program t vl x) = Definition (reidentLower ops n) $ Program (reidentUpper ops t) (map (version ops n) vl) x
makeDefinition ops n b@(Constant _) = Definition (reidentLower ops n) b

definition :: ReidentOps -> Definition -> Definition
definition ops (Definition n d) = makeDefinition ops (reidentUnique ops n) d

reident :: ReidentOptions -> XDR.Scope -> Specification -> Specification
reident o = map . definition . reidentOps o

