module Types
  ( Type (..)
  , Literal (..)
  , Expression (..))where

import Data.Text ( Text )

data TForallInfo = TForallInfo Text Type
  deriving Show

instance Eq TForallInfo where
  (TForallInfo _ t1) == (TForallInfo _ t2) = t1 == t2  

data Type
    = TUnit
    | TInteger
    | TRational
    | TBool
    | TArrow Type Type
    | TVariable Text
    | TForall TForallInfo
    deriving (Eq, Show)

data Literal
    = LUnit
    | LInteger Integer
    | LRational Rational
    | LBool Bool
    deriving Eq

instance Show Literal where
  show LUnit = "()"
  show (LInteger int) = show int
  show (LRational rational) = show rational
  show (LBool bool) = show bool

data Expression
    = ELiteral Literal
    | EVariable Text
    | EAbstraction Text Type Expression
    | EApplication Expression Expression
    | ECondition Expression Expression Expression
    | ETypeAbstraction Text Expression
    | ETypeApplication Expression Type
    deriving (Eq, Show)
   
