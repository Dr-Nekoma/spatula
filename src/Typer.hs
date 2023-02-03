{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RankNTypes #-}
module Typer where

import Types
import Data.List ( find, sortBy )
import Data.Text (pack, Text, unpack, append)
import Text.Printf ( printf )
import Utils ( ResultT, throwError', printWarning )
import Data.Traversable
import qualified Data.Map as Map
import Control.Monad
import SWPrelude()
import Data.Bifunctor ( Bifunctor(second) )
import Control.Monad.IO.Class
import Data.Either.Extra
import qualified Data.Set as S
import qualified Data.Text.Encoding as Option
import Data.Function
import Data.List
import Data.Maybe
import qualified Data.Set as S
import Foreign.C (CBool)
import GHC.Stack (ccsToStrings)
import Data.IntMap.Merge.Lazy (mapWhenMatched)

data TyperEnv = TyperEnv
  { variableTypes :: Map.Map Text Type
  , kindContext :: Map.Map TVariableInfo Kind
  , aliasContext :: Map.Map Text Type
  } deriving (Eq, Show)

-- TODO: We should have warnings if expressions are returning something other than unit

{-
Right now we are following Fsharp and OCaml way of making type aliases, a.k.a, the order does matter.

type a = string (* Initial type alias type *)
  
let x: a = "nathan" (* x is a string *)
  
type a = int (* After this x is still a string *)
-}

data PatternMatchState =
      SSatisfied
    | SUnit
    | SInt [Integer]
    | SRational [Rational]
    | SBool (Maybe Bool)
    | SString [Text]
    | SSum [(Label, [PatternMatchState])]
    deriving (Show, Eq)

createBinds :: Type -> Pattern -> ResultT [(Label, Type)]
createBinds type' (PVariable label) = pure [(label, type')]
createBinds _ PWildcard = pure []
createBinds type' p@(PDisjunctive firstPattern secondPattern) = do
  firstBinds <- createBinds type' firstPattern
  secondBinds <- createBinds type' secondPattern
  if sortBy (compare `on` fst) firstBinds == sortBy (compare `on` fst) secondBinds
    then pure firstBinds
  else throwError' $ printf "TYPE ERROR: Not all the possibilities in the Or pattern %s have the same binds" (show p)
createBinds type' p@(PAs pattern' label) = do
  binds <- createBinds type' pattern'
  case find ((== label) . fst) binds of
    Nothing -> pure $ (label, type') : binds
    Just _ -> throwError' $ printf "TYPE ERROR: Recurrent label identified in pattern %s" (show p)
createBinds (TAlgebraic constructors) (PSumType label patterns) = do
  types <- case find ((== label) . fst) constructors of
             Nothing -> throwError' $ printf "TYPE ERROR: Constructor %s could not be found" (show label)
             Just (_, types) | length types == length patterns -> pure types
                             | otherwise -> throwError' $ printf "TYPE ERROR: List of types %s has a different length than the length of list of patterns %s" (show types) (show patterns)
  binds <- concat <$> zipWithM createBinds types patterns
  foldM_ (\acc el -> if S.member el acc then throwError' "TYPE ERROR: Repeated bind found" else pure $ S.insert el acc) S.empty $ map fst binds
  pure binds
createBinds TInteger (PLiteral (LInteger _)) = pure []
createBinds TRational (PLiteral (LRational _)) = pure []
createBinds TString (PLiteral (LString _)) = pure []
createBinds TBool (PLiteral (LBool _)) = pure []
createBinds TUnit (PLiteral LUnit) = pure []
createBinds type' pattern' = throwError' $ printf "TYPE ERROR: Pattern %s not valid for type %s" (show pattern') (show type')

getInitialPatternMatchState :: Type -> PatternMatchState
getInitialPatternMatchState TUnit = SUnit
getInitialPatternMatchState TInteger = SInt []
getInitialPatternMatchState TRational = SRational []
getInitialPatternMatchState TString = SString []
getInitialPatternMatchState TBool = SBool Nothing
getInitialPatternMatchState (TAlgebraic constructors) = SSum $ map (second $ map getInitialPatternMatchState) constructors
getInitialPatternMatchState _ = error "We have a wrong type here xD"

getNextPatternMatchState :: PatternMatchState -> Pattern -> ResultT PatternMatchState
getNextPatternMatchState SSatisfied pattern' = do
  printWarning $ printf "Pattern %s is unreachable" (show pattern') 
  pure SSatisfied
getNextPatternMatchState _ (PVariable _) = pure SSatisfied
getNextPatternMatchState _ PWildcard = pure SSatisfied
getNextPatternMatchState state (PDisjunctive firstPattern secondPattern) = do
  nextState <- getNextPatternMatchState state firstPattern
  getNextPatternMatchState nextState secondPattern
getNextPatternMatchState state (PAs pattern' _) = getNextPatternMatchState state pattern'
getNextPatternMatchState (SBool Nothing) (PLiteral (LBool value)) = pure $ SBool (Just value)
getNextPatternMatchState state@(SBool (Just pastBool)) pattern'@(PLiteral (LBool value)) = do
  if pastBool == value
  then printWarning (printf "Pattern %s is unreachable" (show pattern')) >> pure state
  else pure SSatisfied
getNextPatternMatchState state@(SInt previousIntegers) pattern'@(PLiteral (LInteger value)) = do
  case find (==value) previousIntegers of
    Just _ -> printWarning (printf "Pattern %s is unreachable" (show pattern')) >> pure state
    Nothing -> pure . SInt $ value : previousIntegers
getNextPatternMatchState (SSum constructorStates) (PSumType label constructorPatterns) = do
   let function (identifier, states) = do
         next <- if identifier == label then zipWithM getNextPatternMatchState states constructorPatterns else pure states
         pure (identifier, next)
   nextConstructorsStates <- mapM function constructorStates
   if all (all (== SSatisfied) . snd) nextConstructorsStates
   then pure SSatisfied
   else pure $ SSum nextConstructorsStates
getNextPatternMatchState state pattern' = throwError' $ printf "TYPE ERROR: Problem with next state function %s %s" (show state) (show pattern')

typeCheckGuard :: TyperEnv -> Maybe Expression -> ResultT ()
typeCheckGuard _ Nothing = pure ()
typeCheckGuard env (Just guard') = do
  typedGuard <- typeCheckExpression env guard'
  case typedGuard of
    TBool -> pure ()
    other -> throwError' $ printf "TYPE ERROR: Encountered guard %s with type %s and it should be a boolean" (show guard') (show other)

checkMatchBody :: Maybe Type -> Type -> ResultT ()
checkMatchBody Nothing _ = pure ()
checkMatchBody (Just expectedType) bodyType = if expectedType == bodyType then pure () else throwError' $ printf "TYPE ERROR: Expected body type %s and found type %s" (show expectedType) (show bodyType)

addFunctionsToEnv :: TyperEnv -> Text -> [(TVariableInfo, Kind)] -> Type -> TyperEnv
addFunctionsToEnv _ _ _ (TAlgebraic []) = error "This should be impossible. Great job Lemos with the Parser"
addFunctionsToEnv env@TyperEnv{..} typeName accum type'@(TAlgebraic list) =
  let makeForall currentType = foldl (\x (info, kind) -> TForall (AbstractionInfo info kind x)) currentType accum
      functions = map (second (makeForall . foldr TArrow (TAlias typeName type'))) list
      addFunction (name, function) acc =
        Map.insert (typeName <> "." <> name) function (Map.insert name function acc)
      newEnv = foldr addFunction variableTypes functions
  in env { variableTypes = newEnv }
addFunctionsToEnv env typeName acc (TAbstraction (AbstractionInfo info kind type')) =
  let newAcc = (info, kind) : acc
  in addFunctionsToEnv env typeName newAcc type'
addFunctionsToEnv env _ _ _ = env

typeCheckDeclarations :: TyperEnv -> [Declaration] -> (Declaration -> Declaration) -> ResultT TyperEnv
typeCheckDeclarations _ [] _ = throwError' "DECLARATION ERROR: No declarations found to type check"
typeCheckDeclarations env@TyperEnv{} list callback = foldM fun env list
  where fun acc (callback -> (DeclExpr expr)) = typeCheckExpression acc expr >> return acc
        fun acc@TyperEnv{..} (callback -> (DeclVal name value)) =
          do type' <- typeCheckExpression acc value
             return $ acc { variableTypes = Map.insert name type' variableTypes}
        fun acc@TyperEnv{..} (callback -> (DeclType name t)) = do
          type' <- findPlaceholderAlias acc t
          kind <- kindCheckWithEnvironment acc type'
          let acc2 = addFunctionsToEnv acc name [] type'
          pure $ acc2 { kindContext = Map.insert (Name name) kind kindContext
                      , aliasContext = Map.insert name type' aliasContext }
        fun acc@TyperEnv{..} (callback -> (DeclFun name expectedType expr)) =
          do type' <- findPlaceholderAlias acc expectedType
             kind <- kindCheckWithEnvironment acc type'
             case kind of
               StarK -> do
                let newEnv = acc { variableTypes = Map.insert name type' variableTypes}
                t <- typeCheckExpression newEnv expr      
                if reduceType t == reduceType type'
                then return newEnv
                else throwError' $ printf "DECLARATION ERROR: Annotated type %s is different than obtained type %s" (show type') (show type')
               other -> throwError' $ printf "DECLARATION ERROR: Annotated type %s has kind %s and it should be *" (show type') (show other)
        fun acc (callback -> (DeclModule name decls)) = typeCheckDeclarations acc decls (renameDeclaration (append name ":"))

findPlaceholderAlias :: TyperEnv -> Type -> ResultT Type
findPlaceholderAlias TyperEnv{..} (TAliasPlaceholder name) =
  case Map.lookup name aliasContext of
    Nothing -> throwError' $ printf "TYPE ERROR: Didn't find alias %s in environment" (show name)
    Just t -> pure $ TAlias name t
findPlaceholderAlias env (TArrow t1 t2) = do
  t1' <- findPlaceholderAlias env t1
  t2' <- findPlaceholderAlias env t2
  pure $ TArrow t1' t2'
findPlaceholderAlias env@TyperEnv{..} (TForall (AbstractionInfo i k t)) = do
  let newEnv = env { aliasContext = Map.insert (extractName i) (TVariable i) aliasContext }
  t' <- findPlaceholderAlias newEnv t
  pure . TForall $ AbstractionInfo i k t'
findPlaceholderAlias env (TApplication t1 t2) = do
  t1' <- findPlaceholderAlias env t1
  t2' <- findPlaceholderAlias env t2
  pure $ TApplication t1' t2'
findPlaceholderAlias env@TyperEnv{..} (TAbstraction (AbstractionInfo i k t)) = do
  let newEnv = env { aliasContext = Map.insert (extractName i) (TVariable i) aliasContext }
  t' <- findPlaceholderAlias newEnv t
  pure . TAbstraction $ AbstractionInfo i k t'
findPlaceholderAlias env (TAnonymousRecord typedNames) = do
  let (names, types) = unzip typedNames
  ts <- for types (findPlaceholderAlias env)
  pure . TAnonymousRecord $ zip names ts
findPlaceholderAlias _ TUnit = pure TUnit
findPlaceholderAlias _ TInteger = pure TInteger
findPlaceholderAlias _ TRational = pure TRational
findPlaceholderAlias _ TBool = pure TBool
findPlaceholderAlias _ TString = pure TString
findPlaceholderAlias _ (TList v) = pure (TList v)
findPlaceholderAlias _ (TVariable v) = pure (TVariable v)
findPlaceholderAlias env (TAlias name type') = do
  t' <- findPlaceholderAlias env type'
  pure $ TAlias name t'
findPlaceholderAlias env (TAlgebraic typedNames) = do
  let (names, types) = unzip typedNames
  ts <- mapM (\ts -> for ts (findPlaceholderAlias env)) types
  pure . TAlgebraic $ zip names ts

typeCheckExpression :: TyperEnv -> Expression -> ResultT Type
typeCheckExpression env (EList list) = do
  let allSameType type' = all (== type')
  listTypes <- for list (fmap reduceType . typeCheckExpression env)
  case listTypes of
    [] -> pure $ (TList . TListInfo) Nothing 
    (x:xs) | allSameType x xs -> pure $ (TList . TListInfo) (Just x)
    (x:_) -> throwError' $ printf "TYPE ERROR: Type mismatch on list. Are all the elements '%s'?" (show x)

typeCheckExpression _ (EAlgebraic _ _) = throwError' "We tried to type check an EAlgebraic"

typeCheckExpression _ (EPatternMatching _ []) = throwError' "This should not be possible. Good job with the parser Lemos"

typeCheckExpression env@TyperEnv{..} (EPatternMatching toMatch list) = do
  type' <- reduceType <$> typeCheckExpression env toMatch
  let folder (expectedType, state) (pattern', guard', body) = do
        binds <- createBinds type' pattern'
        let newEnv = env { variableTypes = foldl f variableTypes binds }
            f acc (label, internalType) = Map.insert label internalType acc
        typeCheckGuard newEnv guard'
        bodyType <- typeCheckExpression newEnv body
        checkMatchBody expectedType bodyType
        nextState <- getNextPatternMatchState state pattern'
        pure (Just bodyType, if isJust guard' then state else nextState)
  (expectedBodyType, state) <- foldM folder (Nothing, getInitialPatternMatchState type') list
  case state of
    SSatisfied -> pure $ fromJust expectedBodyType
    other -> do
      printWarning (printf "Non-exhaustive patterns! Pattern match state ended with %s" (show other))
      pure $ fromJust expectedBodyType

-- TODO add a warning message to the elements that are not Unit type (aside from the last one of course)
typeCheckExpression env (EProgn list) = do
  listTypes <- for list (fmap reduceType . typeCheckExpression env)
  case listTypes of
    [] -> pure TUnit
    nonEmptyList -> pure $ last nonEmptyList

typeCheckExpression env (ERecordProjection expr label) = do
  potentialRecord <- reduceType <$> typeCheckExpression env expr
  case potentialRecord of
    TAnonymousRecord fields -> do
      let fun target (name, _) = name == target
      case find (fun label) fields of
        Nothing ->  throwError' $ printf "TYPE ERROR: Record projection %s could not be found in %s" (unpack label) (show potentialRecord)
        Just (_, type') -> pure type'
    other -> throwError' $ printf "TYPE ERROR: Record projection can only be used on records and got %s" (show other)

typeCheckExpression env (ERecordUpdate expr toUpdateList) = do
  potentialRecord <- reduceType <$> typeCheckExpression env expr
  case potentialRecord of
    TAnonymousRecord fields -> do
      toUpdateTypes <- S.fromList <$> for toUpdateList (mapM (fmap reduceType . typeCheckExpression env))
      let setFields = S.fromList fields
      if toUpdateTypes `S.isSubsetOf` setFields
      then pure $ TAnonymousRecord fields
      else throwError' $ printf "TYPE ERROR: Didn't find fields %s in record update" (show $ S.difference toUpdateTypes setFields)
    other -> throwError' $ printf "TYPE ERROR: Record update can only be used on records and got %s" (show other)

typeCheckExpression env (EAnonymousRecord fields) = do
  let (labels, exprs) = unzip fields
  types <- for exprs (typeCheckExpression env)
  pure $ TAnonymousRecord (sortBy (\(label1, _) (label2, _) -> compare label1 label2) (zip labels types))
  
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
typeCheckExpression env@TyperEnv{..} (EAbstraction label t returnType expression) = do
  type' <- findPlaceholderAlias env t
  parameterKind <- kindCheckWithEnvironment env type'
  case parameterKind of 
    StarK -> do 
      let newEnv = env { variableTypes = Map.insert label type' variableTypes }
      resultType <- typeCheckExpression newEnv expression
      case returnType of
        Just rt -> do potentialAlias <- findPlaceholderAlias env rt
                      annotatedReturnKind <- kindCheckWithEnvironment env potentialAlias
                      case annotatedReturnKind of
                        StarK -> do let reducedAnnotatedType = reduceType potentialAlias
                                        reducedResultType = reduceType resultType
                                    if reducedAnnotatedType == reducedResultType
                                    then pure $ TArrow type' potentialAlias
                                    else throwError' $ printf "TYPE ERROR 1: Body type %s does not match annotated return type %s." (show potentialAlias) (show resultType)
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
  let newKindEnv = env { kindContext = Map.insert label kind kindContext
                       , aliasContext = Map.insert (extractName label) (TVariable label) aliasContext }
  resultType <- typeCheckExpression newKindEnv body
  case returnType of
    Just rt -> do potentialAlias <- findPlaceholderAlias env rt
                  annotatedReturnKind <- kindCheckWithEnvironment newKindEnv potentialAlias
                  case annotatedReturnKind of
                    StarK -> do let reducedAnnotatedType = reduceType potentialAlias
                                    reducedResultType = reduceType resultType
                                if reducedAnnotatedType == reducedResultType
                                then pure $ TForall $ AbstractionInfo label kind potentialAlias
                                else throwError' $ printf "TYPE ERROR 2: Body type %s does not match annotated return type %s." (show potentialAlias) (show resultType)
                    other -> throwError' $ printf "KIND ERROR: Annotated return type should have kind * but it has %s" (show other)
    Nothing -> pure $ TForall $ AbstractionInfo label kind resultType

typeCheckExpression env (ETypeApplication expr type') = do
  reducedFunctionType <- reduceType <$> typeCheckExpression env expr
  case reducedFunctionType of
    TForall (AbstractionInfo identifier kind bodyType) -> do
     potentialAlias <- findPlaceholderAlias env type'
     expectedKind <- kindCheckWithEnvironment env potentialAlias
     if kind == expectedKind
       then pure $ typeSubstitution identifier potentialAlias bodyType
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
    TAliasPlaceholder name ->
      let kind = Map.lookup (Name name) kindContext in
      maybe (throwError' $ printf "KIND ERROR: Unbound type alias %s in the environment." (show name)) return kind
    TAlias name _ -> 
      let kind = Map.lookup (Name name) kindContext in
      maybe (throwError' $ printf "KIND ERROR: Unbound type alias %s in the environment." (show name)) return kind
    TAnonymousRecord fields -> do
      internalKinds <- for (map snd fields) (kindCheckWithEnvironment env)
      if all (==StarK) internalKinds
      then pure StarK
      else throwError' "KIND ERROR: Internal types of fields should have kind *."
    TList (TListInfo Nothing) -> pure StarK
    TList (TListInfo (Just x)) -> do
      internalKind <- kindCheckWithEnvironment env x
      case internalKind of
        StarK -> pure StarK
        other -> throwError' $ printf "KIND ERROR: Internal types of lists should have kind * and this has %s" (show other)
    TVariable label ->
      let kind = Map.lookup label kindContext in
      maybe (throwError' $ printf "KIND ERROR: Unbound type variable %s in the environment." (show label)) return kind
    TArrow input output -> do
      kindInput <- kindCheckWithEnvironment env input
      kindOutput <- kindCheckWithEnvironment env output
      case (kindInput, kindOutput) of
        (StarK, StarK) -> pure StarK
        (left, right) -> throwError' $ printf "KIND ERROR: Expression arrow must have kind * -> * and it has %s -> %s." (show left) (show right)
    TForall (AbstractionInfo identifier kind bodyType) -> do
       let newEnv = Map.insert identifier kind kindContext
       kindBody <- kindCheckWithEnvironment (env {kindContext = newEnv}) bodyType
       case kindBody of
        StarK -> pure StarK
        kind' -> throwError' $ printf "KIND ERROR: Foralls should return * but this has %s." (show kind')
    TApplication abstractionType argumentType -> do
      kindAbs <- kindCheckWithEnvironment env abstractionType
      kindArg <- kindCheckWithEnvironment env argumentType
      case kindAbs of
        (ArrowK k1 k2) -> if k1 == kindArg
                          then pure k2
                          else throwError' $ printf "KIND ERROR: Kind argument %s does not match expected kind %s." (show kindArg) (show k1) 
        kind' -> throwError' $ printf "KIND ERROR: Type abstraction of kind %s is not a arrow kind" (show kind')
    TAbstraction (AbstractionInfo label kind bodyType) -> do
      let newKindEnv = Map.insert label kind kindContext
          newAliasEnv = Map.insert (extractName label) (TVariable label) aliasContext
      kindBody <- kindCheckWithEnvironment (env {kindContext = newKindEnv, aliasContext = newAliasEnv}) bodyType
      pure $ ArrowK kind kindBody
    TAlgebraic typedNames -> do
      let types = map snd typedNames
      internalKinds <- for (concat types) (kindCheckWithEnvironment env)
      if all (==StarK) internalKinds
      then pure StarK
      else throwError' "KIND ERROR: Internal types for ADTs of fields should have kind *."

reduceType :: Type -> Type
reduceType type' = 
  case type' of
    TUnit -> TUnit
    TInteger -> TInteger
    TRational -> TRational
    TBool -> TBool
    TString -> TString
    TAliasPlaceholder _ -> error "This should never happen. Something is broken in reduction xD"
    TAlias _ type'' -> reduceType type''
    TAnonymousRecord fields ->
      TAnonymousRecord $ map (second reduceType) fields
    TAlgebraic namedTypes ->
      let (names, types) = unzip namedTypes
          reducedTypes = map (map reduceType) types
      in TAlgebraic $ zip names reducedTypes
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
