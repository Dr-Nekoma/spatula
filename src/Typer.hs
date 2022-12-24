{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Typer ( typeCheckDeclarations, typeCheckExpression, TyperEnv(..) ) where

import Types
    ( Type(TForall, TUnit, TInteger, TRational, TArrow, TBool, TVariable, TApplication, TAbstraction, TString, TList, TAnonymusRecord),
      TListInfo(..),
      TVariableInfo(..),
      Kind(..),
      Declaration(..),
      Expression(..),
      LetSort(..),
      Literal(LBool, LUnit, LInteger, LRational, LString),
      Operator(..),
      typeSubstitution,
      AbstractionInfo(AbstractionInfo) )
import Data.List ( find, sortBy )
import Data.Text (pack, Text)
import Text.Printf ( printf )
import Utils ( ResultT, throwError' )
import Data.Traversable
import qualified Data.Map as Map
import Control.Monad
import SWPrelude()
import Data.Bifunctor ( Bifunctor(second) )

data TyperEnv = TyperEnv
  { variableTypes :: Map.Map Text Type
  , kindContext :: Map.Map TVariableInfo Kind
  , aliasContext :: Map.Map Text Type
  } deriving (Eq, Show)

-- TODO: We should have warnings if expressions are returning something other than unit
typeCheckDeclarations :: TyperEnv -> [Declaration] -> ResultT TyperEnv
typeCheckDeclarations _ [] = throwError' "DECLARATION ERROR: No declaration found to type check"
typeCheckDeclarations env@TyperEnv{} list = foldM fun env list
  where fun acc (DeclExpr expr) = typeCheckExpression acc expr >> return acc
        fun acc@TyperEnv{..} (DeclVal name value) =
          do type' <- typeCheckExpression acc value
             return $ acc { variableTypes = Map.insert name type' variableTypes}
        fun acc@TyperEnv{..} (DeclType name type') = do
          kind <- kindCheckWithEnvironment acc type'
          pure $ acc { kindContext = Map.insert (Name name) kind kindContext
                     , aliasContext = Map.insert name type' aliasContext }
        fun acc@TyperEnv{..} (DeclFun name expectedType expr) =
          do kind <- kindCheckWithEnvironment acc expectedType
             case kind of
               StarK -> do
                let newEnv = acc { variableTypes = Map.insert name expectedType variableTypes}
                type' <- typeCheckExpression newEnv expr      
                if reduceType type' == reduceType expectedType
                then return newEnv
                else throwError' $ printf "DECLARATION ERROR: Annotated type %s is different than obtained type %s" (show expectedType) (show type')
               other -> throwError' $ printf "DECLARATION ERROR: Annotated type %s has kind %s and it should be *" (show expectedType) (show other)


typeCheckExpression :: TyperEnv -> Expression -> ResultT Type
typeCheckExpression env (EList list) = do
  let allSameType type' = all (== type')
  listTypes <- for list (fmap reduceType . typeCheckExpression env)
  case listTypes of
    [] -> pure $ (TList . TListInfo) Nothing 
    (x:xs) | allSameType x xs -> pure $ (TList . TListInfo) (Just x)
    (x:_) -> throwError' $ printf "TYPE ERROR: Type mismatch on list. Are all the elements '%s'?" (show x)

typeCheckExpression env (EAnonymusRecord fields) = do
  let (labels, exprs) = unzip fields
  types <- for exprs (typeCheckExpression env)
  pure $ TAnonymusRecord (sortBy (\(label1, _) (label2, _) -> compare label1 label2) (zip labels types))
  
typeCheckExpression _ (ELiteral literal) =
  case literal of
    LUnit -> pure TUnit
    LInteger _ -> pure TInteger
    LRational _ -> pure TRational
    LBool _ -> pure TBool
    LString _ -> pure TString

typeCheckExpression TyperEnv{..} (EVariable label) =
  case Map.lookup label variableTypes of
    Nothing -> throwError' $ printf "TYPE ERROR: Unbound variable %s in the environment." label
    Just type' -> pure type'

typeCheckExpression env@TyperEnv{..} (ELet In bindings body) = do
  let (labels, expressions) = unzip bindings
  typedExpressions <- for expressions (typeCheckExpression env)
  let newEnv = foldl f variableTypes (zip labels typedExpressions)
      f acc (label, type') = Map.insert label type' acc
  typeCheckExpression (env {variableTypes = newEnv}) body

typeCheckExpression env (ELet Plus [] body) = typeCheckExpression env body
typeCheckExpression env@TyperEnv{..} (ELet Plus ((label, expr):xs) body) = do
  typedExpression <- typeCheckExpression env expr
  typeCheckExpression (env { variableTypes = Map.insert label typedExpression variableTypes}) (ELet Plus xs body)

-- TODO: We should kind check the return type in the type annotation to provide better error messages
typeCheckExpression env@TyperEnv{..} (EAbstraction label type' returnType expression) = do
  parameterKind <- kindCheckWithEnvironment env type'
  case parameterKind of 
    StarK -> do 
      let newEnv = env { variableTypes = Map.insert label type' variableTypes }
      resultType <- typeCheckExpression newEnv expression
      case returnType of
        Just rt -> do annotatedReturnKind <- kindCheckWithEnvironment env rt
                      case annotatedReturnKind of
                        StarK -> do let reducedAnnotatedType = reduceType rt
                                        reducedResultType = reduceType resultType
                                    if reducedAnnotatedType == reducedResultType
                                    then pure $ TArrow type' rt
                                    else throwError' $ printf "TYPE ERROR: Body type %s does not match annotated return type %s." (show rt) (show returnType)
                        other -> throwError' $ printf "KIND ERROR: Annotated return type should have kind * but it has %s" (show other)
        Nothing -> pure $ TArrow type' resultType
    other -> throwError' $ printf "KIND ERROR: Expected parameter to have kind * but it has %s." (show other)

typeCheckExpression env (EApplication fun arg) = do
  reducedFunType <- reduceType <$> typeCheckExpression env fun
  case reducedFunType of
    TArrow parameterType resultType -> do
      reducedArgType <- reduceType <$> typeCheckExpression env arg
      let reducedParameterType = reduceType parameterType
      if reducedArgType == reducedParameterType
        then pure resultType
      else throwError' $ printf "TYPE ERROR: Type mismatch between parameter of type %s and argument of type %s." (show parameterType) (show reducedArgType)
    _ -> throwError' $ printf "TYPE ERROR: Attempted to apply a value %s that it is not a function." (show reducedFunType)

typeCheckExpression env (ECondition cond thenBranch elseBranch) = do
  condType <- typeCheckExpression env cond
  let reducedCondType = reduceType condType
  case reducedCondType of
    TBool -> do
      thenBranchType <- typeCheckExpression env thenBranch
      elseBranchType <- typeCheckExpression env elseBranch
      let reducedThenType = reduceType thenBranchType
          reducedElseType = reduceType elseBranchType
      if reducedThenType == reducedElseType
        then pure thenBranchType
        else throwError' $ printf "TYPE ERROR: Type mismatch between then branch of type %s and else branch of type %s." (show thenBranchType) (show elseBranchType)
    _ -> throwError' $ printf "TYPE ERROR: Predicate of type %s needs to be a boolean in if-expression." (show condType)

-- TODO: We should kind check the return type in the type annotation to provide better error messages
typeCheckExpression env@TyperEnv{..} (ETypeAbstraction label kind returnType body) = do
  let newKindEnv = env { kindContext = Map.insert label kind kindContext }
  resultType <- typeCheckExpression newKindEnv body
  case returnType of
    Just rt -> do annotatedReturnKind <- kindCheckWithEnvironment newKindEnv rt
                  case annotatedReturnKind of
                    StarK -> do let reducedAnnotatedType = reduceType rt
                                    reducedResultType = reduceType resultType
                                if reducedAnnotatedType == reducedResultType
                                then pure $ TForall $ AbstractionInfo label kind rt
                                else throwError' $ printf "TYPE ERROR: Body type %s does not match annotated return type %s." (show rt) (show returnType)
                    other -> throwError' $ printf "KIND ERROR: Annotated return type should have kind * but it has %s" (show other)
    Nothing -> pure $ TForall $ AbstractionInfo label kind resultType

typeCheckExpression env (ETypeApplication expr type') = do
  reducedFunctionType <- reduceType <$> typeCheckExpression env expr
  case reducedFunctionType of
    TForall (AbstractionInfo identifier kind bodyType) -> do
     expectedKind <- kindCheckWithEnvironment env type'
     if kind == expectedKind
       then pure $ typeSubstitution identifier type' bodyType
       else throwError' $ printf "KIND ERROR: Expected kind %s for type application does not match with %s." (show expectedKind) (show kind)
    _ -> throwError' $ printf "TYPE ERROR: Cannot do a type application with a value of type %s that is not a type abstraction." (show reducedFunctionType)
    
typeCheckExpression _ (EOperation _ []) = throwError' "TYPE ERROR: Operators don't type check with no elements"

-- TODO: Check properly the arithmetic operators with the exhaustiveness -> THIS IS A TRAP
typeCheckExpression env (EOperation operator list@(_:_)) = do
  operandsTypes <- for list (fmap reduceType . typeCheckExpression env)
  let checkIfAll type' = all (== type')
      x = head operandsTypes
      xs = tail operandsTypes
      checkIfList (TList _) = True
      checkIfList _ = False
      checkIfJust (TList (TListInfo (Just _))) = True
      checkIfJust _ = False

  case operator of
    OpAnd -> if checkIfAll TBool operandsTypes then pure TBool else throwError' "TYPE ERROR: And operator asks for booleans."
    OpOr  -> if checkIfAll TBool operandsTypes then pure TBool else throwError' "TYPE ERROR: Or operator asks for booleans."
    OpConcat -> 
      case find (not . checkIfList) operandsTypes of
        Just different -> 
          throwError' $ printf "TYPE ERROR: Expected a List|_| but found '%s'" (show different)
        Nothing ->
          case filter checkIfJust operandsTypes of
            (y:ys) ->
              case find (/= y) ys of
                Just different -> 
                  throwError' $ printf "TYPE ERROR: Attempting to concat lists with distinct types. Received '%s' while expected 's'." (show different) (show y)
                Nothing -> pure y
            _ -> pure $ (TList . TListInfo) Nothing
    OpEqual -> do
      case find (/= x) xs of
        Just firstDifferent -> throwError' $ printf "TYPE ERROR: Mismatch between elements at an equality comparison. Expected '%s' but got '%s'" (show x) (show firstDifferent)
        Nothing -> pure TBool
    arithmetics -- Arithmetic
      | checkIfAll TRational operandsTypes -> pure $ if operator == OpLessThan then TBool else TRational
      | checkIfAll TInteger operandsTypes  -> pure $ if operator == OpLessThan then TBool else TInteger
      | otherwise -> throwError' $ printf "TYPE ERROR: Arithmetic operator %s must use only numbers of the same sort." (show arithmetics)

kindCheckWithEnvironment :: TyperEnv -> Type -> ResultT Kind
kindCheckWithEnvironment env@TyperEnv{..} type' =
  case type' of
    TUnit -> pure StarK
    TInteger -> pure StarK
    TRational -> pure StarK
    TBool -> pure StarK
    TString -> pure StarK
    TAnonymusRecord fields -> do
      let ifStar StarK = True
          ifStar _ = False
      internalKinds <- for (map snd fields) (kindCheckWithEnvironment env)
      if all ifStar internalKinds
      then pure StarK
      else throwError' "Internal types of fields should have kind *."
    TList (TListInfo Nothing) -> pure StarK
    TList (TListInfo (Just x)) -> do
      internalKind <- kindCheckWithEnvironment env x
      case internalKind of
        StarK -> pure StarK
        other -> throwError' $ printf "Internal types of lists should have kind * and this has %s" (show other)
    TVariable label ->
      let kind = Map.lookup label kindContext in
      maybe (throwError' $ printf "Unbound type variable %s in the environment." (show label)) return kind
    TArrow input output -> do
      kindInput <- kindCheckWithEnvironment env input
      kindOutput <- kindCheckWithEnvironment env output
      case (kindInput, kindOutput) of
        (StarK, StarK) -> pure StarK
        (left, right) -> throwError' $ printf "Expression arrow must have kind * -> * and it has %s -> %s." (show left) (show right)
    TForall (AbstractionInfo identifier kind bodyType) -> do
       let newEnv = Map.insert identifier kind kindContext
       kindBody <- kindCheckWithEnvironment (env {kindContext = newEnv}) bodyType
       case kindBody of
        StarK -> pure StarK
        kind' -> throwError' $ printf "Foralls should return * but this has %s." (show kind')
    TApplication abstractionType argumentType -> do
      kindAbs <- kindCheckWithEnvironment env abstractionType
      kindArg <- kindCheckWithEnvironment env argumentType
      case kindAbs of
        (ArrowK k1 k2) -> if k1 == kindArg
                          then pure k2
                          else throwError' $ printf "Kind argument %s does not match expected kind %s." (show kindArg) (show k1) 
        kind' -> throwError' $ printf "Type abstraction of kind %s is not a arrow kind" (show kind')
    TAbstraction (AbstractionInfo label kind bodyType) -> do
      let newEnv = Map.insert label kind kindContext
      kindBody <- kindCheckWithEnvironment (env {kindContext = newEnv}) bodyType
      pure $ ArrowK kind kindBody      

reduceType :: Type -> Type
reduceType type' = 
  case type' of
    TUnit -> TUnit
    TInteger -> TInteger
    TRational -> TRational
    TBool -> TBool
    TString -> TString
    TAnonymusRecord fields ->
      TAnonymusRecord $ map (second reduceType) fields
    TList (TListInfo type'') -> TList . TListInfo $ fmap reduceType type''
    TArrow parameter returnType -> TArrow (reduceType parameter) (reduceType returnType)
    TVariable ident -> TVariable ident
    TForall (AbstractionInfo parameterName parameterKind bodyType) ->
      let tForall = reduceType bodyType
      in TForall
          (AbstractionInfo
            parameterName
            parameterKind
            tForall)
    TAbstraction (AbstractionInfo parameterName parameterKind body) -> 
      TAbstraction (AbstractionInfo parameterName parameterKind (reduceType body))
    TApplication function_ argument_ -> 
      let function = reduceType function_
          argument = reduceType argument_
      in    
      case function of
        TAbstraction (AbstractionInfo parameterName _ body) -> reduceType $ typeSubstitution parameterName argument body
        _ -> TApplication function argument
