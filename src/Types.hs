{-# LANGUAGE DeriveGeneric #-}
module Types
  (
    typeSubstitution
  , TForallInfo(..)
  , Type(..)
  , Literal(..)
  , Expression(..)
  )
where

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
    TForall info@(TForallInfo identifier type'') | identifier == placeHolder -> TForall info
                                                 | otherwise -> TForall (TForallInfo identifier (typeSubstitution placeHolder type' type''))
    TVariable identifier | identifier == placeHolder -> type'
                         | otherwise -> TVariable identifier
    TUnit -> TUnit
    TInteger -> TInteger
    TRational -> TRational
    TBool -> TBool

data TForallInfo = TForallInfo Text Type
  deriving (Generic, Show)

instance Arbitrary TForallInfo where
  arbitrary = genericArbitrary

instance ToADTArbitrary TForallInfo

instance Eq TForallInfo where
  (TForallInfo ident1 type1) == (TForallInfo ident2 type2) =
    if ident1 == ident2
    then type1 == type2
    else type1 == typeSubstitution ident2 (TVariable ident1) type2

data Type
    = TUnit
    | TInteger
    | TRational
    | TBool
    | TArrow Type Type
    | TVariable Text
    | TForall TForallInfo
    deriving (Generic, Eq, Show)

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

data Expression
    = ELiteral Literal
    | EVariable Text
    | EAbstraction Text Type (Maybe Type) Expression
    | EApplication Expression Expression
    | ECondition Expression Expression Expression
    | ETypeAbstraction Text Expression
    | ETypeApplication Expression Type
    deriving (Generic, Eq, Show)

instance Arbitrary Expression where
  arbitrary = genericArbitrary

instance ToADTArbitrary Expression
