{-# LANGUAGE DeriveGeneric #-}
module Types where

import Data.Text.Arbitrary ( Text )
import GHC.Generics ( Generic )
import Test.QuickCheck ( Arbitrary(arbitrary) )
import Test.QuickCheck.Arbitrary.ADT
    ( ToADTArbitrary, genericArbitrary )

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

class Curryable a where  
    kurry :: a -> a -> a

data Kind =
    StarK
  | ArrowK Kind Kind
  deriving (Eq, Generic, Show)

instance Curryable Kind where
  kurry = ArrowK

instance Arbitrary Kind where
  arbitrary = genericArbitrary

data TForallInfo = TForallInfo Text Kind Type
  deriving (Generic, Show)

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
    | TArrow Type Type
    | TVariable Text
    | TForall TForallInfo
    | TApplication Type Type
    | TAbstraction Text Kind Type
    deriving (Generic, Eq, Show)

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
    deriving (Generic, Eq, Show)

instance Arbitrary Literal where
  arbitrary = genericArbitrary

instance ToADTArbitrary Literal

-- instance Show Literal where
--   show LUnit = "()"
--   show (LInteger int) = show int
--   show (LRational rational) = show rational
--   show (LBool bool) = show bool

data LetSort = In | Plus
  deriving (Generic, Eq, Show)

instance Arbitrary LetSort where
  arbitrary = genericArbitrary

data Expression
    = ELiteral Literal
    | EVariable Text
    | ELet LetSort [(Text, Expression)] Expression -- What about kind checking this?
    | EAbstraction Text Type (Maybe Type) Expression
    | EApplication Expression Expression
    | ECondition Expression Expression Expression
    | ETypeAbstraction Text Kind (Maybe Type) Expression
    | ETypeApplication Expression Type
    deriving (Generic, Eq, Show)

instance Arbitrary Expression where
  arbitrary = genericArbitrary

instance ToADTArbitrary Expression
