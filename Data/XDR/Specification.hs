-- | XDR: External Data Representation specification

module Data.XDR.Specification
  where

import qualified Data.XDR.Types as XDR

type Identifier = String
type Length = XDR.UnsignedInt

maxLength :: Length
maxLength = maxBound

data ArrayLength
  = FixedArray    { arrayLength :: !Length }
  | VariableArray { arrayLength :: !Length -- ^defaulted to maxLength
    }
  deriving (Show)

data TypeDescriptor
  = TypeSingle
    { descriptorType :: !TypeSpecifier
    }
  | TypeArray 
    { descriptorType :: !TypeSpecifier
    , descriptorLength :: !ArrayLength
    }
  | TypeOpaque
    { descriptorLength :: !ArrayLength
    }
  | TypeString
    { descriptorLength :: !ArrayLength -- ^only 'VariableArray'
    }
  | TypeOptional
    { descriptorType :: !TypeSpecifier
    }
  deriving (Show)

data TypeSpecifier
  = TypeInt
  | TypeUnsignedInt
  | TypeHyper
  | TypeUnsignedHyper
  | TypeFloat
  | TypeDouble
  | TypeQuadruple
  | TypeBool
  | TypeEnum !EnumBody
  | TypeStruct !StructBody
  | TypeUnion !UnionBody
  | TypeIdentifier !Identifier
  deriving (Show)

-- |Non-void declaration
data Declaration = Declaration
  { declarationIdentifier :: !Identifier
  , declarationType :: TypeDescriptor
  }
  deriving (Show)

-- |'Declaration' or void
type OptionalDeclaration = Maybe Declaration

newtype EnumBody = EnumBody
  { enumValues :: [(Identifier, XDR.Int)]
  }
  deriving (Show)

newtype StructBody = StructBody
  { structMembers :: [Declaration] -- ^with voids elided
  }
  deriving (Show)

data UnionBody = UnionBody
  { unionDiscriminant :: !Declaration
  , unionCases :: [(Integer, OptionalDeclaration)]
  , unionDefault :: Maybe OptionalDeclaration
  }
  deriving (Show)

data Definition
  = TypeDef
    { definitionIdentifier :: !Identifier
    , definitionType :: TypeDescriptor
    }
  | Constant
    { definitionIdentifier :: !Identifier
    , definitionConstant :: Integer
    }
  deriving (Show)

type Specification = [Definition]

baseSpecification :: Specification
baseSpecification =
  [ TypeDef "bool" $ TypeSingle $ TypeEnum $ EnumBody [("FALSE", 0), ("TRUE", 1)]
  ]
