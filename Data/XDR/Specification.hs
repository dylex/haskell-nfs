-- | XDR specification, as per RFC4506

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

type EnumValues = [(Identifier, XDR.Int)]

newtype EnumBody = EnumBody
  { enumValues :: EnumValues
  }
  deriving (Show)

boolValues :: EnumValues
boolValues = [("FALSE", 0), ("TRUE", 1)]

newtype StructBody = StructBody
  { structMembers :: [Declaration] -- ^with voids elided
  }
  deriving (Show)

data UnionArm = UnionArm
  { unionCase :: !Integer
  , unionCaseLiteral :: String -- ^The literal string found after "case", for labeling
  , unionDeclaration :: OptionalDeclaration
  }
  deriving (Show)

data UnionBody = UnionBody
  { unionDiscriminant :: !Declaration
  , unionCases :: [UnionArm]
  , unionDefault :: Maybe OptionalDeclaration
  }
  deriving (Show)

data DefinitionBody
  = TypeDef TypeDescriptor
  | Constant Integer
  deriving (Show)

data Definition = Definition
  { definitionIdentifier :: !Identifier
  , definitionBody :: !DefinitionBody
  }
  deriving (Show)

type Specification = [Definition]
