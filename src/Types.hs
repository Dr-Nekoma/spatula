{-# LANGUAGE DeriveGeneric #-}
module Types ( 
  typeSubstitution, 
  Curryable(..), 
  Kind(..), 
  Type(..), 
  TForallInfo(..), 
  Literal(..),
  LetSort(..),
  Operator(..),
  Expression(..)) where

import Data.Text.Arbitrary ( Text )
import Data.Text ( unpack )
import GHC.Generics ( Generic )
import Test.QuickCheck ( Arbitrary(arbitrary) )
import Test.QuickCheck.Arbitrary.ADT
    ( ToADTArbitrary, genericArbitrary )
import Text.Printf ( printf )

typeSubstitution :: Text -> Type -> Type -> Type
typeSubstitution placeHolder type' target =
  case target of
    TArrow parameter returnType ->
      TArrow
      (typeSubstitution placeHolder type' parameter)
      (typeSubstitution placeHolder type' returnType)
    TApplication abstractionType argumentType ->
      TApplication
        (typeSubstitution placeHolder type' abstractionType)
        (typeSubstitution placeHolder type' argumentType)
    typeAbstraction@(TAbstraction label kind type'')
      | label == placeHolder ->
          typeAbstraction
      | otherwise ->
          TAbstraction label kind (typeSubstitution placeHolder type' type'')
    TForall info@(TForallInfo identifier kind type'')
      | identifier == placeHolder -> TForall info
      | otherwise -> TForall (TForallInfo identifier kind (typeSubstitution placeHolder type' type''))
    TVariable identifier | identifier == placeHolder -> type'
                         | otherwise -> TVariable identifier
    TUnit -> TUnit
    TInteger -> TInteger
    TRational -> TRational
    TBool -> TBool
    TString -> TString
    TList list -> TList $ fmap (typeSubstitution placeHolder type') list
      

class Curryable a where  
    kurry :: a -> a -> a

data Kind =
    StarK
  | ArrowK Kind Kind
  deriving (Eq, Generic)

instance Show Kind where
  show StarK = "*"
  show (ArrowK kind1 kind2) = show kind1 ++ " -> " ++ show kind2

instance Curryable Kind where
  kurry = ArrowK

instance Arbitrary Kind where
  arbitrary = genericArbitrary

data TForallInfo = TForallInfo Text Kind Type
  deriving (Generic)

instance Show TForallInfo where
  show (TForallInfo label kind type') = printf "%s. %s; %s" (unpack label) (show kind) (show type')

instance Arbitrary TForallInfo where
  arbitrary = genericArbitrary

instance ToADTArbitrary TForallInfo

instance Eq TForallInfo where
  (TForallInfo ident1 kind1 type1) == (TForallInfo ident2 kind2 type2) =
    if kind1 == kind2 then
      if ident1 == ident2
      then type1 == type2
      else type1 == typeSubstitution ident2 (TVariable ident1) type2
    else False

data Type
    = TUnit
    | TInteger
    | TRational
    | TBool
    | TString
    | TList (Maybe Type)
    | TArrow Type Type
    | TVariable Text
    | TForall TForallInfo
    | TApplication Type Type
    | TAbstraction Text Kind Type
    deriving (Generic, Eq)

instance Show Type where
  show TUnit = "Unit"
  show TInteger = "Integer"
  show TRational = "Rational"
  show TBool = "Bool"
  show TString = "String"
  show (TList (Just type')) = printf "List|%s|" (show type')
  show (TList Nothing) = "List|_|"
  show (TArrow source target) = printf "%s -> %s" (show source) (show target)
  show (TVariable label) = unpack label
  show (TForall info) = "forall " ++ show info
  show (TApplication fun arg) = printf "%s %s" (show fun) (show arg)
  show (TAbstraction label kind type') = printf "lambda %s : %s -> %s" (unpack label) (show kind) (show type')

instance Curryable Type where
  kurry = TArrow

instance Arbitrary Type where
  arbitrary = genericArbitrary

instance ToADTArbitrary Type

data Literal
    = LUnit
    | LInteger Integer
    | LRational Rational
    | LBool Bool
    | LString Text
--    | LTuple [Expression]
    deriving (Generic, Eq)

instance Arbitrary Literal where
  arbitrary = genericArbitrary

instance ToADTArbitrary Literal

instance Show Literal where
  show LUnit = "()"
  show (LInteger int) = show int
  show (LRational rational) = show rational
  show (LBool bool) = show bool
  show (LString string) = unpack string

data LetSort = In | Plus
  deriving (Generic, Eq, Show)

data Operator = OpConcat | OpPlus | OpMinus | OpDiv | OpMul | OpAnd | OpOr | OpEqual | OpLessThan
  deriving (Generic, Eq, Enum, Bounded)

instance Show Operator where
  show OpPlus  = "+"
  show OpMinus = "-"
  show OpMul   = "*"
  show OpDiv   = "/"
  show OpAnd   = "and"
  show OpOr    = "or"
  show OpEqual = "="
  show OpConcat = "^"
  show OpLessThan = "<"

instance Arbitrary LetSort where
  arbitrary = genericArbitrary

instance Arbitrary Operator where
  arbitrary = genericArbitrary

data Expression
    = ELiteral Literal
    | EVariable Text
    | EOperation Operator [Expression]
    | ELet LetSort [(Text, Expression)] Expression -- What about kind checking this?
    | EAbstraction Text Type (Maybe Type) Expression
    | EApplication Expression Expression
    | ECondition Expression Expression Expression
    | ETypeAbstraction Text Kind (Maybe Type) Expression
    | ETypeApplication Expression Type
    | EList [Expression]
    deriving (Generic, Eq, Show)

instance Arbitrary Expression where
  arbitrary = genericArbitrary
