{-# LANGUAGE OverloadedStrings #-}
module Evaluator
  ( eval
  , Literal (..)
  , Expression (..)) where

import qualified Data.Map as Map
import Data.Text ( Text )
import Data.ByteString.Builder.Prim (condB)

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
    | ECondition Expression Expression Expression
    | ETypeAbstraction Text Expression
    | ETypeApplication Expression Type

data Value
    = VUnit
    | VLiteral Literal
    | VClosure Text Expression Env
    | VNativeFunction (Value -> Result)

eval :: Env -> Expression -> Result

eval _ (ELiteral literal) = pure $ VLiteral literal

eval env (EVariable label) =
  case Map.lookup label env of
    Nothing -> Left "Couldn't find your variable in the environment"
    Just value -> pure value

eval env (EAbstraction label _ body) =
  pure $ VClosure label body env

eval env (EApplication fun arg) = do
  funValue <- eval env fun
  argValue <- eval env arg
  case funValue of
    VClosure label body closedEnv ->
      let newEnv = Map.insert label argValue closedEnv in
        eval newEnv body
    VNativeFunction fun ->
      fun argValue
    _ -> Left "Failed attempting to apply a value to something that is not a function."

eval env (ECondition cond thenBranch elseBranch) = do
  test <- eval env cond
  case test of
    VLiteral (LBool b) ->
      if b then
        eval env thenBranch
      else
        eval env elseBranch
    _ -> Left "Condition expression could not be evaluated to a boolean."
  
eval env (ETypeAbstraction _ body) =
  eval env body

eval env (ETypeApplication expr _) =
  eval env expr
