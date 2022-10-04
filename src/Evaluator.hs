{-# LANGUAGE OverloadedStrings #-}
module Evaluator
  ( eval
  , Literal (..)
  , Expression (..)) where

import qualified Data.Map as Map
import Data.Text ( Text )

type Env = Map.Map Text Value
type Result = Either Text Value

data Type
    = TUnit
    | TInteger
    | TRational
    | TBool
    | TArrow Type Type
    | TForall Text Type
    | TVariable Text

data Literal
    = LUnit
    | LInteger Integer
    | LRational Rational
    | LBool Bool

data Expression
    = ELiteral Literal
    | EVariable Text
    | EAbstraction Text Type Expression
    | EApplication Expression Expression
    | ETypeAbstraction Text Expression
    | ETypeApplication Expression Type

data Value
    = VUnit
    | VLiteral Literal
    | VClosure Text Type Expression Env
    | VNativeFunction Value Value

eval :: Env -> Expression -> Result
eval _ (ELiteral literal) = pure $ VLiteral literal
eval env (EVariable label) =
  case Map.lookup label env of
    Nothing -> Left "Couldn't find your variable in the environment"
    Just value -> pure value
eval _ _ = undefined

