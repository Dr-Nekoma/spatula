{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Typer ( typeCheck ) where

import Control.Monad
import Types
    ( Type(TForall, TUnit, TInteger, TRational, TArrow, TBool, TVariable, TApplication, TAbstraction, TString, TList),
      Kind(..),
      Expression(..),
      LetSort(..),
      Literal(LBool, LUnit, LInteger, LRational, LString),
      Operator(..),
      typeSubstitution,
      TForallInfo(TForallInfo) )
import Data.List ( find )
import Data.Text (pack, Text)
import Text.Printf ( printf )
import Utils ( Result )
import Data.Traversable
import qualified Data.Map as Map
import SWPrelude

data TyperEnv = TyperEnv
  { variableTypes :: Map.Map Text Type
  , kindContext :: Map.Map Text Kind
  } deriving (Eq, Show)

typeCheckWithEnvironment :: TyperEnv -> Expression -> Result Type
typeCheckWithEnvironment env (EList list) =
  let allSameType type' = all (== type')
  in case for list (reduceType env <=< typeCheckWithEnvironment env) of
    Right [] -> pure $ TList Nothing 
    Right (x:xs) | allSameType x xs -> pure $ TList (Just x)
    Right (x:_) -> Left $ pack $ printf "TYPE ERROR: Type mismatch on list. Are all the elements '%s'?" (show x)
    Left e -> Left e

typeCheckWithEnvironment _ (ELiteral literal) =
  case literal of
    LUnit -> pure TUnit
    LInteger _ -> pure TInteger
    LRational _ -> pure TRational
    LBool _ -> pure TBool
    LString _ -> pure TString

typeCheckWithEnvironment TyperEnv{..} (EVariable label) =
  case Map.lookup label variableTypes of
    Nothing -> Left $ pack $ printf "TYPE ERROR: Unbound variable %s in the environment." label
    Just type' -> pure type'

typeCheckWithEnvironment env@TyperEnv{..} (ELet In bindings body) = do
  let (labels, expressions) = unzip bindings
  typedExpressions <- for expressions (typeCheckWithEnvironment env)
  let newEnv = foldl f variableTypes (zip labels typedExpressions)
      f acc (label, type') = Map.insert label type' acc
  typeCheckWithEnvironment (env {variableTypes = newEnv}) body

typeCheckWithEnvironment env (ELet Plus [] body) = typeCheckWithEnvironment env body
typeCheckWithEnvironment env@TyperEnv{..} (ELet Plus ((label, expr):xs) body) = do
  typedExpression <- typeCheckWithEnvironment env expr
  typeCheckWithEnvironment (env { variableTypes = Map.insert label typedExpression variableTypes}) (ELet Plus xs body)

-- TODO: We should kind check the return type in the type annotation to provide better error messages
typeCheckWithEnvironment env@TyperEnv{..} (EAbstraction label type' returnType expression) = do
  parameterKind <- kindCheckWithEnvironment env type'
  case parameterKind of 
    StarK -> do 
      let newEnv = env { variableTypes = Map.insert label type' variableTypes }
      resultType <- typeCheckWithEnvironment newEnv expression
      case returnType of
        Just rt -> do annotatedReturnKind <- kindCheckWithEnvironment env rt
                      case annotatedReturnKind of
                        StarK -> do reducedAnnotatedType <- reduceType env rt
                                    reducedResultType <- reduceType env resultType
                                    if reducedAnnotatedType == reducedResultType
                                    then pure $ TArrow type' rt
                                    else Left $ pack $ printf "TYPE ERROR: Body type %s does not match annotated return type %s." (show rt) (show returnType)
                        other -> Left $ pack $ printf "KIND ERROR: Annotated return type should have kind * but it has %s" (show other)
        Nothing -> pure $ TArrow type' resultType
    other -> Left $ pack $ printf "KIND ERROR: Expected parameter to have kind * but it has %s." (show other)

typeCheckWithEnvironment env (EApplication fun arg) = do
  reducedFunType <- reduceType env =<< typeCheckWithEnvironment env fun
  case reducedFunType of
    TArrow parameterType resultType -> do
      reducedArgType <- reduceType env =<< typeCheckWithEnvironment env arg
      reducedParameterType <- reduceType env parameterType
      if reducedArgType == reducedParameterType
        then pure resultType
      else Left $ pack $ printf "TYPE ERROR: Type mismatch between parameter of type %s and argument of type %s." (show parameterType) (show reducedArgType)
    _ -> Left $ pack $ printf "TYPE ERROR: Attempted to apply a value %s that it is not a function." (show reducedFunType)

typeCheckWithEnvironment env (ECondition cond thenBranch elseBranch) = do
  condType <- typeCheckWithEnvironment env cond
  reducedCondType <- reduceType env condType
  case reducedCondType of
    TBool -> do
      thenBranchType <- typeCheckWithEnvironment env thenBranch
      elseBranchType <- typeCheckWithEnvironment env elseBranch
      reducedThenType <- reduceType env thenBranchType
      reducedElseType <- reduceType env elseBranchType
      if reducedThenType == reducedElseType
        then pure thenBranchType
        else Left $ pack $ printf "TYPE ERROR: Type mismatch between then branch of type %s and else branch of type %s." (show thenBranchType) (show elseBranchType)
    _ -> Left $ pack $ printf "TYPE ERROR: Predicate of type %s needs to be a boolean in if-expression." (show condType)

-- TODO: We should kind check the return type in the type annotation to provide better error messages
typeCheckWithEnvironment env@TyperEnv{..} (ETypeAbstraction label kind returnType body) = do
  let newKindEnv = env { kindContext = Map.insert label kind kindContext }
  resultType <- typeCheckWithEnvironment newKindEnv body
  case returnType of
    Just rt -> do annotatedReturnKind <- kindCheckWithEnvironment newKindEnv rt
                  case annotatedReturnKind of
                    StarK -> do reducedAnnotatedType <- reduceType newKindEnv rt
                                reducedResultType <- reduceType newKindEnv resultType
                                if reducedAnnotatedType == reducedResultType
                                then pure $ TForall $ TForallInfo label kind rt
                                else Left $ pack $ printf "TYPE ERROR: Body type %s does not match annotated return type %s." (show rt) (show returnType)
                    other -> Left $ pack $ printf "KIND ERROR: Annotated return type should have kind * but it has %s" (show other)
    Nothing -> pure $ TForall $ TForallInfo label kind resultType

typeCheckWithEnvironment env (ETypeApplication expr type') = do
  reducedFunctionType <- reduceType env =<< typeCheckWithEnvironment env expr
  case reducedFunctionType of
    TForall (TForallInfo identifier kind bodyType) -> do
     expectedKind <- kindCheckWithEnvironment env type'
     if kind == expectedKind
       then pure $ typeSubstitution identifier type' bodyType
       else Left $ pack $ printf "KIND ERROR: Expected kind %s for type application does not match with %s." (show expectedKind) (show kind)
    _ -> Left $ pack $ printf "TYPE ERROR: Cannot do a type application with a value of type %s that is not a type abstraction." (show reducedFunctionType)
    
typeCheckWithEnvironment _ (EOperation _ []) = Left "TYPE ERROR: Operators don't type check with no elements"

-- TODO: Check properly the arithmetic operators with the exhaustiveness -> THIS IS A TRAP
typeCheckWithEnvironment env (EOperation operator list@(_:_)) = do
  operandsTypes <- for list (reduceType env <=< typeCheckWithEnvironment env)
  let checkIfAll type' = all (== type')
      x = head operandsTypes
      xs = tail operandsTypes
      checkIfList (TList _) = True
      checkIfList _ = False
      checkIfJust (TList (Just _)) = True
      checkIfJust _ = False

  case operator of
    OpAnd -> if checkIfAll TBool operandsTypes then pure TBool else Left "TYPE ERROR: And operator asks for booleans."
    OpOr  -> if checkIfAll TBool operandsTypes then pure TBool else Left "TYPE ERROR: Or operator asks for booleans."
    OpConcat -> 
      case find (not . checkIfList) operandsTypes of
        Just different -> 
          Left $ pack $ printf "TYPE ERROR: Expected a List|_| but found '%s'" (show different)
        Nothing ->
          case filter checkIfJust operandsTypes of
            (y:ys) ->
              case find (/= y) ys of
                Just different -> 
                  Left $ pack $ printf "TYPE ERROR: Attempting to concat lists with distinct types. Received '%s' while expected 's'." (show different) (show y)
                Nothing -> pure y
            _ -> pure $ TList Nothing
    OpEqual -> do
      case find (/= x) xs of
        Just firstDifferent -> Left $ pack $ printf "TYPE ERROR: Mismatch between elements at an equality comparison. Expected '%s' but got '%s'" (show x) (show firstDifferent)
        Nothing -> pure x
    arithmetics -- Arithmetic
      | checkIfAll TRational operandsTypes -> pure $ if operator == OpLessThan then TBool else TRational
      | checkIfAll TInteger operandsTypes  -> pure $ if operator == OpLessThan then TBool else TInteger
      | otherwise -> Left $ pack $ printf "TYPE ERROR: Arithmetic operator %s must use only numbers of the same sort." (show arithmetics)
      
typeCheck :: Expression -> Result Type
typeCheck = typeCheckWithEnvironment (TyperEnv typerPrelude Map.empty)

kindCheckWithEnvironment :: TyperEnv -> Type -> Result Kind
kindCheckWithEnvironment env@TyperEnv{..} type' =
  case type' of
    TUnit -> pure StarK
    TInteger -> pure StarK
    TRational -> pure StarK
    TBool -> pure StarK
    TString -> pure StarK
    TList Nothing -> pure StarK
    TList (Just x) -> do
      internalKind <- kindCheckWithEnvironment env x
      case internalKind of
        StarK -> pure StarK
        other -> Left $ pack $ printf "Internal types of lists should have kind * and this has %s" (show other)
    TVariable label ->
      let kind = Map.lookup label kindContext in
      maybe (Left $ pack $ printf "Unbound type variable %s in the environment." (show label)) Right kind
    TArrow input output -> do
      kindInput <- kindCheckWithEnvironment env input
      kindOutput <- kindCheckWithEnvironment env output
      case (kindInput, kindOutput) of
        (StarK, StarK) -> pure StarK
        (left, right) -> Left $ pack $ printf "Expression arrow must have kind * -> * and it has %s -> %s." (show left) (show right)
    TForall (TForallInfo identifier kind bodyType) -> do
       let newEnv = Map.insert identifier kind kindContext
       kindBody <- kindCheckWithEnvironment (env {kindContext = newEnv}) bodyType
       case kindBody of
        StarK -> pure StarK
        kind' -> Left $ pack $ printf "Foralls should return * but this has %s." (show kind')
    TApplication abstractionType argumentType -> do
      kindAbs <- kindCheckWithEnvironment env abstractionType
      kindArg <- kindCheckWithEnvironment env argumentType
      case kindAbs of
        (ArrowK k1 k2) -> if k1 == kindArg
                          then pure k2
                          else Left $ pack $ printf "Kind argument %s does not match expected kind %s." (show kindArg) (show k1) 
        kind' -> Left $ pack $ printf "Type abstraction of kind %s is not a arrow kind" (show kind')
    TAbstraction label kind bodyType -> do
      let newEnv = Map.insert label kind kindContext
      kindBody <- kindCheckWithEnvironment (env {kindContext = newEnv}) bodyType
      pure $ ArrowK kind kindBody      

reduceType :: TyperEnv -> Type -> Either Text Type
reduceType env type' = 
  case type' of
    TUnit -> pure TUnit
    TInteger -> pure TInteger
    TRational -> pure TRational
    TBool -> pure TBool
    TString -> pure TString
    TList elements -> pure $ TList elements
    TArrow parameter returnType -> do
      start <- reduceType env parameter
      target <- reduceType env returnType
      pure $ TArrow start target
    TVariable ident -> pure $ TVariable ident
    TForall (TForallInfo parameterName parameterKind bodyType) -> do
      tForall <- reduceType env bodyType
      pure $ TForall
          (TForallInfo
            parameterName
            parameterKind
            tForall)
    TAbstraction parameterName parameterKind body -> do
      bodyType <- reduceType env body
      pure $ TAbstraction parameterName parameterKind bodyType
    TApplication function_ argument_ -> do
      function <- reduceType env function_
      argument <- reduceType env argument_
      argumentKind <- kindCheckWithEnvironment env argument
      case function of
        TAbstraction parameterName parameterKind body ->
          if argumentKind == parameterKind then
            pure $ typeSubstitution parameterName argument body
          else error "TODO TApplication"
        other -> Left $ pack $ printf "Expected a type abtraction but got %s" (show other)


-- (λx -> (λy -> y) x)
