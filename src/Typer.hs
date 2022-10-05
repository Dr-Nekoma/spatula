{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Typer ( typeCheck ) where

import Types
    ( Expression(..),
      Literal(LBool, LUnit, LInteger, LRational),
      Type(TBool, TUnit, TInteger, TRational, TArrow),
      equal )
import Data.Text ( Text )
import Data.Set ( Set )
import qualified Data.Map as Map

type TyperResult = Either Text Type
data TyperEnv = TyperEnv
  { boundedTypes :: Set Text
  , variableTypes :: Map.Map Text Type
  } deriving (Eq, Show)

typeCheck :: TyperEnv -> Expression -> TyperResult

typeCheck _ (ELiteral literal) =
  let getType = case literal of
                 LUnit -> TUnit
                 LInteger _ -> TInteger
                 LRational _ -> TRational
                 LBool _ -> TBool
  in pure getType

typeCheck TyperEnv{..} (EVariable label) =
  case Map.lookup label variableTypes of
    Nothing -> Left "Could not find yours variable's type in the environment"
    Just type' -> pure type'

typeCheck env (EAbstraction label type' expression) = do
  let newEnv = Map.insert label type' (variableTypes env)
  resultType <- typeCheck (env {variableTypes = newEnv}) expression
  pure $ TArrow type' resultType

typeCheck env (EApplication fun arg) = do
  funType <- typeCheck env fun
  case funType of
    TArrow parameterType resultType -> do
      argType <- typeCheck env arg
      if equal argType parameterType
        then pure resultType
      else Left "Type mismatch between parameter and argument"
    _ -> Left "Failed attempting to type check something that is not a function"

typeCheck env (ECondition cond thenBranch elseBranch) = do
  condType <- typeCheck env cond
  case condType of
    TBool -> do
      thenType <- typeCheck env thenBranch
      elseType <- typeCheck env elseBranch
      if equal thenType elseType
        then pure thenType
        else Left "Type mismatch between branches in condition"
    _ -> Left "Predicate needs to be a boolean in condition"

typeCheck env (ETypeAbstraction label body) = undefined

typeCheck env (ETypeApplication expr type') = undefined
