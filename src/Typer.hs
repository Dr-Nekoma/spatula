{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Typer ( typeCheck ) where

import Types
    ( Type(TForall, TUnit, TInteger, TRational, TArrow, TBool, TVariable, TApplication, TAbstraction),
      Kind(..),
      Expression(..),
      Literal(LBool, LUnit, LInteger, LRational),
      typeSubstitution,
      TForallInfo(TForallInfo) )
import Data.Text ( Text )
import Utils ( Result )
import qualified Data.Map as Map

data TyperEnv = TyperEnv
  { variableTypes :: Map.Map Text Type
  , kindContext :: Map.Map Text Kind
  } deriving (Eq, Show)

typeCheckWithEnvironment :: TyperEnv -> Expression -> Result Type

typeCheckWithEnvironment _ (ELiteral literal) =
  let getType = case literal of
                 LUnit -> TUnit
                 LInteger _ -> TInteger
                 LRational _ -> TRational
                 LBool _ -> TBool
  in pure getType

typeCheckWithEnvironment TyperEnv{..} (EVariable label) =
  case Map.lookup label variableTypes of
    Nothing -> Left "Could not find your variable's type in the environment"
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

-- TODO: We need to do reduction at some point due to the potential type of expr  
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

typeCheckWithEnvironment env@TyperEnv{..} (ETypeAbstraction label kind body) = do
  let newKindEnv = Map.insert label kind kindContext
  bodyType <- typeCheckWithEnvironment (env { kindContext = newKindEnv}) body
  pure $ TForall $ TForallInfo label kind bodyType

-- TODO: We need to do reduction at some point due to the potential type of expr  
typeCheckWithEnvironment env (ETypeApplication expr type') = do
  functionType <- typeCheckWithEnvironment env expr
  case functionType of
    TForall (TForallInfo identifier kind identType) -> do
     expectedKind <- kindCheckWithEnvironment env type'
     if kind == expectedKind
       then pure $ typeSubstitution identifier type' identType
       else Left "Expected kind for type application does not match"
    _ -> Left "Cannot do a type application with a value that is not a type abstraction"

typeCheck :: Expression -> Result Type
typeCheck = typeCheckWithEnvironment (TyperEnv Map.empty Map.empty)

kindCheckWithEnvironment :: TyperEnv -> Type -> Result Kind
kindCheckWithEnvironment env@TyperEnv{..} type' =
  case type' of
    TUnit -> pure StarK
    TInteger -> pure StarK
    TRational -> pure StarK
    TBool -> pure StarK
    TVariable label ->
      let kind = Map.lookup label kindContext in
      maybe (Left "Unbound type variable.") Right kind
    TArrow input output -> do
      kindInput <- kindCheckWithEnvironment env input
      kindOutput <- kindCheckWithEnvironment env output
      case (kindInput, kindOutput) of
        (StarK, StarK) -> pure StarK
        _ -> Left "Expression arrow must have kind * and it has something different"
    TForall (TForallInfo identifier kind identType) -> do
       let newEnv = Map.insert identifier kind kindContext
       kindIdent <- kindCheckWithEnvironment (env {kindContext = newEnv}) identType
       case kindIdent of
        StarK -> pure StarK
        _ -> Left "Foralls should return * but this has something different"
    TApplication abstractionType argumentType -> do
      kindAbs <- kindCheckWithEnvironment env abstractionType
      kindArg <- kindCheckWithEnvironment env argumentType
      case kindAbs of
        (ArrowK k1 k2) -> if k1 == kindArg
                          then pure k2
                          else Left "Kinds don't match in TApplication"
        _ -> Left "Type abstraction is not a arrow kind"
    TAbstraction label kind bodyType -> do
      let newEnv = Map.insert label kind kindContext
      kindBody <- kindCheckWithEnvironment (env {kindContext = newEnv}) bodyType
      pure $ ArrowK kind kindBody      
