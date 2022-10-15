{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Typer ( typeCheck ) where

import Types
    ( Type(TForall, TUnit, TInteger, TRational, TArrow, TBool),
      Expression(..),
      Literal(LBool, LUnit, LInteger, LRational),
      typeSubstitution,
      TForallInfo(TForallInfo) )
import Data.Text ( Text )
import qualified Data.Set as Set
import qualified Data.Map as Map

type TyperResult = Either Text Type
data TyperEnv = TyperEnv
  { boundedTypes :: Set.Set Text
  , variableTypes :: Map.Map Text Type
  } deriving (Eq, Show)

typeCheckWithEnvironment :: TyperEnv -> Expression -> TyperResult

typeCheckWithEnvironment _ (ELiteral literal) =
  let getType = case literal of
                 LUnit -> TUnit
                 LInteger _ -> TInteger
                 LRational _ -> TRational
                 LBool _ -> TBool
  in pure getType

typeCheckWithEnvironment TyperEnv{..} (EVariable label) =
  case Map.lookup label variableTypes of
    Nothing -> Left "Could not find yours variable's type in the environment"
    Just type' -> pure type'

-- Remember to consider that the syntax receives multiples parameters and it does a transformation to curried notation
-- and it has an optional annotated return type
typeCheckWithEnvironment env (EAbstraction label type' returnType expression) = do
  let newEnv = Map.insert label type' (variableTypes env)
  resultType <- typeCheckWithEnvironment (env {variableTypes = newEnv}) expression
  let possibleReturn = pure $ TArrow type' resultType
  case returnType of
        Just rt -> if rt == resultType
                      then possibleReturn
                      else Left "Body type does not match annotated return type"
        Nothing -> possibleReturn

typeCheckWithEnvironment env (EApplication fun arg) = do
  funType <- typeCheckWithEnvironment env fun
  case funType of
    TArrow parameterType resultType -> do
      argType <- typeCheckWithEnvironment env arg
      if argType == parameterType
        then pure resultType
      else Left "Type mismatch between parameter and argument"
    _ -> Left "Failed attempting to type check something that is not a function"

typeCheckWithEnvironment env (ECondition cond thenBranch elseBranch) = do
  condType <- typeCheckWithEnvironment env cond
  case condType of
    TBool -> do
      thenType <- typeCheckWithEnvironment env thenBranch
      elseType <- typeCheckWithEnvironment env elseBranch
      if thenType == elseType
        then pure thenType
        else Left "Type mismatch between branches in condition"
    _ -> Left "Predicate needs to be a boolean in condition"

typeCheckWithEnvironment env@TyperEnv{..} (ETypeAbstraction label body) = do
  let newBoundedTypes = Set.insert label boundedTypes
  bodyType <- typeCheckWithEnvironment (env { boundedTypes = newBoundedTypes}) body
  pure $ TForall $ TForallInfo label bodyType
  
typeCheckWithEnvironment env (ETypeApplication expr type') = do
  functionType <- typeCheckWithEnvironment env expr
  case functionType of
    TForall (TForallInfo identifier identType) -> do
      pure $ typeSubstitution identifier type' identType
    _ -> Left "Cannot do a type application with a value that is not a type abstraction"

typeCheck :: Expression -> TyperResult
typeCheck = typeCheckWithEnvironment (TyperEnv Set.empty Map.empty)
