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
import Utils ( ResultT, throwError', throwError'', throwErrorMessage, printWarning, printWarning' )
import Parser
import Data.Traversable
import qualified Data.Map as Map
import Control.Monad
import SWPrelude()
import Data.Bifunctor ( Bifunctor(first, second) )
import Data.Either
import Control.Monad.IO.Class
import Control.Monad.Extra
import Data.Either.Extra
import qualified Data.Set as S
import qualified Data.Text.Encoding as Option
import Data.Function
import Data.List
import Data.Maybe
import qualified Data.Set as S
import Text.Parsec (parse)
import Options.Applicative (metavar)

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
createBinds type'@(FullNode _ _) (FullNode _ (PVariable label)) = pure [(label, type')]
createBinds _ (FullNode _ PWildcard) = pure []
createBinds type' p@(FullNode meta (PDisjunctive firstPattern secondPattern)) = do
  firstBinds <- createBinds type' firstPattern
  secondBinds <- createBinds type' secondPattern
  if sortOn fst firstBinds == sortOn fst secondBinds
    then pure firstBinds
  else throwError'' meta $ printf "TYPE ERROR: Not all the possibilities in the Or pattern %s have the same binds" (show p)
createBinds type' (FullNode meta p@(PAs pattern' label)) = do
  binds <- createBinds type' pattern'
  case find ((== label) . fst) binds of
    Nothing -> pure $ (label, type') : binds
    Just _ -> throwError'' meta $ printf "TYPE ERROR: Recurrent label identified in pattern %s" (show p)
createBinds (FullNode _ (TAlgebraic constructors))(FullNode meta (PSumType label patterns)) = do
  types <- case find ((== label) . fst) constructors of
             Nothing -> throwError'' meta $ printf "TYPE ERROR: Constructor %s could not be found" (show label)
             Just (_, types) | length types == length patterns -> pure types
                             | otherwise -> throwError'' meta $ printf "TYPE ERROR: List of types %s has a different length than the length of list of patterns %s" (show types) (show patterns)
  binds <- concat <$> zipWithM createBinds types patterns
  foldM_ (\acc el -> if S.member el acc then throwError'' (getMetadata el) "TYPE ERROR: Repeated bind found" else pure $ S.insert el acc) S.empty $ map fst binds
  pure binds
createBinds (FullNode _ TInteger) (FullNode _ (PLiteral (LInteger _))) = pure []
createBinds (FullNode _ TRational) (FullNode _ (PLiteral (LRational _))) = pure []
createBinds (FullNode _ TString) (FullNode _ (PLiteral (LString _))) = pure []
createBinds (FullNode _ TBool) (FullNode _ (PLiteral (LBool _))) = pure []
createBinds (FullNode _ TUnit) (FullNode _ (PLiteral LUnit)) = pure []
createBinds type' pattern' = throwError'' (getMetadata pattern') $ printf "TYPE ERROR: Pattern %s not valid for type %s" (show pattern') (show type')

getInitialPatternMatchState :: Type -> PatternMatchState
getInitialPatternMatchState (FullNode _ TUnit) = SUnit
getInitialPatternMatchState (FullNode _ TInteger) = SInt []
getInitialPatternMatchState (FullNode _ TRational) = SRational []
getInitialPatternMatchState (FullNode _ TString) = SString []
getInitialPatternMatchState (FullNode _ TBool) = SBool Nothing
getInitialPatternMatchState (FullNode _ (TAlgebraic constructors)) = SSum $ map (second $ map getInitialPatternMatchState) constructors
getInitialPatternMatchState _ = error "We have a wrong type here xD"

getNextPatternMatchState :: PatternMatchState -> Pattern -> ResultT PatternMatchState
getNextPatternMatchState SSatisfied pattern' = do
  printWarning pattern' $ printf "Pattern %s is unreachable" (show pattern') 
  pure SSatisfied
getNextPatternMatchState _ (FullNode _ (PVariable _)) = pure SSatisfied
getNextPatternMatchState _ (FullNode _ PWildcard) = pure SSatisfied
getNextPatternMatchState state (FullNode _ (PDisjunctive firstPattern secondPattern)) = do
  nextState <- getNextPatternMatchState state firstPattern
  getNextPatternMatchState nextState secondPattern
getNextPatternMatchState state (FullNode _ (PAs pattern' _)) = getNextPatternMatchState state pattern'
getNextPatternMatchState (SBool Nothing) (FullNode _ (PLiteral (LBool value))) = pure $ SBool (Just value)
getNextPatternMatchState state@(SBool (Just pastBool)) pattern'@(FullNode _ (PLiteral ((LBool value)))) = do
  if pastBool == value
  then printWarning pattern' (printf "Pattern %s is unreachable" (show pattern')) >> pure state
  else pure SSatisfied
getNextPatternMatchState state@(SInt previousIntegers) pattern'@(FullNode _ (PLiteral (LInteger value))) = do
  case find (==value) previousIntegers of
    Just _ -> printWarning pattern' (printf "Pattern %s is unreachable" (show pattern')) >> pure state
    Nothing -> pure . SInt $ value : previousIntegers
getNextPatternMatchState (SSum constructorStates) (FullNode _ (PSumType label constructorPatterns)) = do
   let function element@(identifier, states) = do
         if identifier == label
           then do next <- zipWithM getNextPatternMatchState states constructorPatterns
                   if all (== SSatisfied) next
                     then pure Nothing
                     else pure $ Just (identifier, next)
           else pure $ Just element
   nextConstructorStates <- mapMaybeM function constructorStates
   if null nextConstructorStates
   then pure SSatisfied
   else pure $ SSum nextConstructorStates
getNextPatternMatchState state pattern' = throwError'' (getMetadata pattern') $ printf "TYPE ERROR: Problem with next state function %s %s" (show state) (show pattern')

typeCheckGuard :: TyperEnv -> Maybe Expression -> ResultT ()
typeCheckGuard _ Nothing = pure ()
typeCheckGuard env (Just guard') = do
  typedGuard <- typeCheckExpression env guard'
  case typedGuard of
    (FullNode _ TBool) -> pure ()
    other@(FullNode meta _) -> throwError'' meta $ printf "TYPE ERROR: Encountered guard %s with type %s and it should be a boolean" (show guard') (show other)

checkMatchBody :: Maybe Type -> Type -> ResultT ()
checkMatchBody Nothing _ = pure ()
checkMatchBody (Just expectedType) bodyType@(FullNode meta _) =
  if expectedType == bodyType
  then pure ()
  else throwError'' meta $ printf "TYPE ERROR: Expected body type %s and found type %s" (show expectedType) (show bodyType)

addFunctionsToEnv :: TyperEnv -> Text -> [(TVariableInfo, Kind)] -> Type -> TyperEnv
addFunctionsToEnv _ _ _ (FullNode _ (TAlgebraic [])) = error "This should be impossible. Great job Lemos with the Parser"
addFunctionsToEnv env@TyperEnv{..} typeName accum type'@(FullNode _ (TAlgebraic list)) =
  let makeForall currentType = foldl (\x (info, kind) -> makeEmptyNode . TForall $ makeEmptyNode (AbstractionInfo' info kind x)) currentType accum
      functions = map (second (makeForall . foldr (\acc el -> makeEmptyNode $ TArrow acc el) (makeEmptyNode (TAlias (makeEmptyNode typeName) type')))) list
      addFunction (FullNode _ name, function) acc =
        Map.insert (typeName <> "." <> name) function (Map.insert name function acc)
      newEnv = foldr addFunction variableTypes functions
  in env { variableTypes = newEnv }
addFunctionsToEnv env typeName acc (FullNode _ (TAbstraction (FullNode _ (AbstractionInfo' info kind type')))) =
  let newAcc = (info, kind) : acc
  in addFunctionsToEnv env typeName newAcc type'
addFunctionsToEnv env _ _ _ = env

typeCheckDeclarations :: TyperEnv -> [Declaration] -> ResultT TyperEnv
typeCheckDeclarations _ [] = error "DECLARATION ERROR: No declarations found to type check"
typeCheckDeclarations env list = foldM fun env list
  where fun acc decl = fromRight acc <$> typeCheckDeclaration acc decl

checkExistence :: Text -> TyperEnv -> ResultT ()
checkExistence name TyperEnv{..} = do
  if Map.member name aliasContext
    then throwErrorMessage $ printf "DECLARATION ERROR: Label %s already found in alias context." (unpack name)
  else pure ()

loadFile :: TyperEnv -> FullNode FilePath -> ResultT (Either Type TyperEnv)
loadFile env metaFilepath = do
  let meta = getMetadata metaFilepath
      filepath = removeMetadata metaFilepath
  result <- liftIO $ customParse fileP filepath
  case result of
    Left errorParse -> throwError'' meta $ printf "DECLARATION ERROR: Could not load file %s. Error % was found" (filepath) (show errorParse)
    Right (FullNode  _ decls) -> Right <$> foldM (\acc decl -> fromRight acc <$> typeCheckDeclaration acc decl) env decls

typeCheckDeclaration :: TyperEnv -> Declaration -> ResultT (Either Type TyperEnv)
typeCheckDeclaration env (FullNode _ (DeclExpr expr)) = Left <$> typeCheckExpression env expr
typeCheckDeclaration env (FullNode _ (DeclLoad filepath)) = loadFile env filepath
typeCheckDeclaration env@TyperEnv{..} (FullNode _ (DeclVal (FullNode _ name) value)) = do 
          checkExistence name env
          type' <- typeCheckExpression env value
          pure . Right $ env { variableTypes = Map.insert name type' variableTypes}
typeCheckDeclaration env@TyperEnv{..} (FullNode _ (DeclType (FullNode meta name) t)) = do
          checkExistence name env
          type' <- findPlaceholderAlias env t
          kind <- kindCheckWithEnvironment env type'
          let newEnv = addFunctionsToEnv env name [] type'
          pure . Right $ newEnv { kindContext = Map.insert (FullNode meta (Name name)) kind kindContext
                                , aliasContext = Map.insert name type' aliasContext }
typeCheckDeclaration env@TyperEnv{..} (FullNode  _ (DeclFun (FullNode _ name) expectedType expr)) = do            
          checkExistence name env
          type' <- findPlaceholderAlias env expectedType
          kind <- kindCheckWithEnvironment env type'
          case kind of
            (FullNode _ StarK) -> do
              let newEnv = env { variableTypes = Map.insert name type' variableTypes}
              t <- typeCheckExpression newEnv expr      
              if reduceType t == reduceType type'
                then pure $ Right newEnv
                else throwError'' (getMetadata type') $ printf "DECLARATION ERROR: Annotated type %s is different than obtained type %s" (show type') (show t)
            (FullNode meta other) -> throwError'' meta $ printf "DECLARATION ERROR: Annotated type %s has kind %s and it should be *" (show type') (show other)
typeCheckDeclaration _ (FullNode _ (DeclModule _ _)) = error "TODO: Can't type checkMatchBody module declaration"            

findPlaceholderAlias :: TyperEnv -> Type -> ResultT Type
findPlaceholderAlias TyperEnv{..} (FullNode meta (TAliasPlaceholder (FullNode _ name))) =
  case Map.lookup name aliasContext of
    Nothing -> throwError'' meta $ printf "TYPE ERROR: Didn't find alias %s in environment" (show name)
    Just t -> pure . FullNode meta $ TAlias (FullNode meta name) t
findPlaceholderAlias env (FullNode meta (TArrow t1 t2)) = do
  t1' <- findPlaceholderAlias env t1
  t2' <- findPlaceholderAlias env t2
  pure . FullNode meta $ TArrow t1' t2'
findPlaceholderAlias env@TyperEnv{..} (FullNode meta (TForall (FullNode meta' (AbstractionInfo' i k t)))) = do
  let newEnv = env { aliasContext = Map.insert (extractName $ removeMetadata i) (FullNode meta' (TVariable i)) aliasContext }
  t' <- findPlaceholderAlias newEnv t
  pure . FullNode meta . TForall . FullNode meta' $ AbstractionInfo' i k t'
findPlaceholderAlias env (FullNode meta (TApplication t1 t2)) = do
  t1' <- findPlaceholderAlias env t1
  t2' <- findPlaceholderAlias env t2
  pure . FullNode meta $ TApplication t1' t2'
findPlaceholderAlias env@TyperEnv{..} (FullNode meta (TAbstraction (FullNode meta' (AbstractionInfo' i k t)))) = do
  let newEnv = env { aliasContext = Map.insert (extractName $ removeMetadata i) (FullNode meta' (TVariable i)) aliasContext }
  t' <- findPlaceholderAlias newEnv t
  pure . FullNode meta . TAbstraction . FullNode meta' $ AbstractionInfo' i k t'
findPlaceholderAlias env (FullNode meta (TNominalRecord name typedNames)) = do
  let (names, types) = unzip typedNames
  ts <- for types (findPlaceholderAlias env)
  pure . FullNode meta $ TNominalRecord name $ zip names ts
findPlaceholderAlias env (FullNode meta (TAnonymousRecord typedNames)) = do
  let (names, types) = unzip typedNames
  ts <- for types (findPlaceholderAlias env)
  pure . FullNode meta $ TAnonymousRecord $ zip names ts
findPlaceholderAlias _ p@(FullNode _ TUnit) = pure p
findPlaceholderAlias _ p@(FullNode _ TInteger) = pure p
findPlaceholderAlias _ p@(FullNode _ TRational) = pure p
findPlaceholderAlias _ p@(FullNode _ TBool) = pure p
findPlaceholderAlias _ p@(FullNode _ TString) = pure p
findPlaceholderAlias _ p@(FullNode _ (TList _)) = pure p
findPlaceholderAlias _ p@(FullNode _ (TVariable _)) = pure p
findPlaceholderAlias env (FullNode meta (TAlias name type')) = do
  t' <- findPlaceholderAlias env type'
  pure . FullNode meta $ TAlias name t'
findPlaceholderAlias env (FullNode meta (TAlgebraic typedNames)) = do
  let (names, types) = unzip typedNames
  ts <- mapM (\ts -> for ts (findPlaceholderAlias env)) types
  pure . FullNode meta $ TAlgebraic $ zip names ts

typeCheckExpression :: TyperEnv -> Expression -> ResultT Type
typeCheckExpression env (FullNode meta (EList list)) = do
  let allSameType type' = all (== type')
  listTypes <- for list (fmap reduceType . typeCheckExpression env)
  case listTypes of
    [] -> pure . FullNode meta $ (TList . FullNode meta . TListInfo') Nothing 
    (x:xs) | allSameType x xs -> pure . FullNode meta $ (TList . FullNode meta . TListInfo') (Just x)
    (x:_) -> throwError'' meta $ printf "TYPE ERROR: Type mismatch on list. Are all the elements '%s'?" (show x)

typeCheckExpression _ (FullNode meta (EAlgebraic _ _)) = throwError'' meta "We tried to type check an EAlgebraic"

typeCheckExpression env@TyperEnv{..} (FullNode _ (ENominalRecord t fields)) = do
  type' <- findPlaceholderAlias env t
  void $ kindCheckWithEnvironment env type'
  case reduceType type' of
    record@(FullNode meta (TNominalRecord _ savedFields)) -> do 
      test <- typeCheckExpression env (FullNode meta (EAnonymousRecord fields))
      case test of
        (FullNode meta' (TAnonymousRecord fields)) -> do
          let reducedSavedFields = sortOn fst $ map (second reduceType) savedFields
              reducedFields = map (second reduceType) fields
          if reducedSavedFields == reducedFields
            then pure type'
            else throwError'' meta' $ printf "TYPE ERROR: Type mismatch between record declaration and instance %s" (show record)
        (FullNode meta' other) -> throwError'' meta' "TYPE ERROR: There is something wrong with your fields bud"
    (FullNode meta' other) -> throwError'' meta' $ printf "TYPE ERROR: Expected record type but got %s" (show other)

typeCheckExpression env@TyperEnv{..} (FullNode meta (EPatternMatching toMatch list)) = do
  type' <- reduceType <$> typeCheckExpression env toMatch
  let folder (expectedType, state) (pattern', guard', body) = do
        binds <- createBinds type' pattern'
        let newEnv = env { variableTypes = foldl f variableTypes $ map (first removeMetadata) binds }
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
      printWarning' meta (printf "Non-exhaustive patterns! Pattern match state ended with %s" (show other))
      pure $ fromJust expectedBodyType

-- TODO add a warning message to the elements that are not Unit type (aside from the last one of course)
typeCheckExpression env (FullNode meta (EProgn list)) = do
  listTypes <- for list (fmap reduceType . typeCheckExpression env)
  case listTypes of
    [] -> pure $ FullNode meta TUnit
    nonEmptyList -> pure $ last nonEmptyList

typeCheckExpression env (FullNode _ (ERecordProjection expr label)) = do
  potentialRecord <- reduceType <$> typeCheckExpression env expr
  case potentialRecord of
    (FullNode meta (TAnonymousRecord fields)) -> do
      let fun target (name, _) = name == target
      case find (fun label) fields of
        Nothing ->  throwError'' meta $ printf "TYPE ERROR: Record projection %s could not be found in %s" (unpack $ removeMetadata label) (show potentialRecord)
        Just (_, type') -> pure type'
    (FullNode meta (TNominalRecord _ fields)) -> do
      let fun target (name, _) = name == target
      case find (fun label) fields of
        Nothing ->  throwError'' meta $ printf "TYPE ERROR: Record projection %s could not be found in %s" (unpack $ removeMetadata label) (show potentialRecord)
        Just (_, type') -> pure type'
    (FullNode meta other) -> throwError'' meta $ printf "TYPE ERROR: Record projection can only be used on records and got %s" (show other)

typeCheckExpression env (FullNode _ (ERecordUpdate expr toUpdateList)) = do
  potentialRecord <- reduceType <$> typeCheckExpression env expr
  case potentialRecord of
    (FullNode meta (TAnonymousRecord fields)) -> do
      toUpdateTypes <- S.fromList <$> for toUpdateList (mapM (fmap reduceType . typeCheckExpression env))
      let setFields = S.fromList fields
      if toUpdateTypes `S.isSubsetOf` setFields
      then pure . FullNode meta $ TAnonymousRecord fields
      else throwError'' meta $ printf "TYPE ERROR: Didn't find fields %s in record update" (show $ S.difference toUpdateTypes setFields)
    (FullNode meta (TNominalRecord recordName fields)) -> do
      toUpdateTypes <- S.fromList <$> for toUpdateList (mapM (fmap reduceType . typeCheckExpression env))
      let setFields = S.fromList fields
      if toUpdateTypes `S.isSubsetOf` setFields
      then pure . FullNode meta $ TNominalRecord recordName fields
      else throwError'' meta $ printf "TYPE ERROR: Didn't find fields %s in record update" (show $ S.difference toUpdateTypes setFields)
    (FullNode meta other) -> throwError'' meta $ printf "TYPE ERROR: Record update can only be used on records and got %s" (show other)

typeCheckExpression env (FullNode meta (EAnonymousRecord fields)) = do
  let (labels, exprs) = unzip fields
  types <- for exprs (typeCheckExpression env)
  pure . FullNode meta $ TAnonymousRecord (sortOn fst (zip labels types))
  
typeCheckExpression _ (FullNode meta (ELiteral literal)) =
  case literal of
    LUnit -> pure $ FullNode meta TUnit
    LInteger _ -> pure $ FullNode meta TInteger
    LRational _ -> pure $ FullNode meta TRational
    LBool _ -> pure $ FullNode meta TBool
    LString _ -> pure $ FullNode meta TString

typeCheckExpression TyperEnv{..} (FullNode meta (EVariable label)) =
  case Map.lookup (removeMetadata label) variableTypes of
    Nothing -> throwError'' meta $ printf "TYPE ERROR: Unbound variable %s in the environment." (removeMetadata label)
    Just type' -> pure type'

typeCheckExpression env@TyperEnv{..} (FullNode _ (ELet (FullNode _ In) bindings body)) = do
  let (labels, expressions) = unzip bindings
  typedExpressions <- for expressions (typeCheckExpression env)
  let newEnv = foldl f variableTypes (zip (map removeMetadata labels) typedExpressions)
      f acc (label, type') = Map.insert label type' acc
  typeCheckExpression (env {variableTypes = newEnv}) body

typeCheckExpression env (FullNode _ (ELet (FullNode _ Plus) [] body)) = typeCheckExpression env body
typeCheckExpression env@TyperEnv{..} (FullNode meta (ELet (FullNode meta' Plus) ((label, expr):xs) body)) = do
  typedExpression <- typeCheckExpression env expr
  typeCheckExpression (env { variableTypes = Map.insert (removeMetadata label) typedExpression variableTypes}) (FullNode meta (ELet (FullNode meta' Plus) xs body))

-- TODO: We should kind check the return type in the type annotation to provide better error messages
typeCheckExpression env@TyperEnv{..} (FullNode _ (EAbstraction label t returnType expression)) = do
  type' <- findPlaceholderAlias env t
  parameterKind <- kindCheckWithEnvironment env type'
  case parameterKind of 
    (FullNode meta' StarK) -> do 
      let newEnv = env { variableTypes = Map.insert (removeMetadata label) type' variableTypes }
      resultType <- typeCheckExpression newEnv expression
      case returnType of
        Just rt -> do potentialAlias <- findPlaceholderAlias env rt
                      annotatedReturnKind <- kindCheckWithEnvironment env potentialAlias
                      case annotatedReturnKind of
                        (FullNode meta StarK) -> do
                          let reducedAnnotatedType = reduceType potentialAlias
                              reducedResultType = reduceType resultType
                          if reducedAnnotatedType == reducedResultType
                          then pure . FullNode meta $ TArrow type' potentialAlias
                          else throwError'' (getMetadata potentialAlias) $ printf "TYPE ERROR 1: Body type %s does not match annotated return type %s." (puts resultType) (puts potentialAlias)
                        (FullNode meta other) -> throwError'' meta $ printf "KIND ERROR: Annotated return type should have kind * but it has %s" (show other)
        Nothing -> pure . FullNode meta' $ TArrow type' resultType
    (FullNode meta' other) -> throwError'' meta' $ printf "KIND ERROR: Expected parameter to have kind * but it has %s." (show other)

typeCheckExpression env (FullNode meta (EApplication fun arg)) = do
  reducedFunType <- reduceType <$> typeCheckExpression env fun
  case reducedFunType of
    (FullNode meta' (TArrow parameterType resultType)) -> do
      reducedArgType <- reduceType <$> typeCheckExpression env arg
      let reducedParameterType = reduceType parameterType
      if reducedArgType == reducedParameterType
        then pure resultType
      else do
        throwError'' (getMetadata fun) $ printf "TYPE ERROR: Type mismatch between parameter of type %s and argument of type %s." (puts parameterType) (puts reducedArgType)
    _ -> throwError'' meta $ printf "TYPE ERROR: Attempted to apply a value %s that it is not a function." (puts reducedFunType)

typeCheckExpression env (FullNode meta (ECondition cond thenBranch elseBranch)) = do
  condType <- typeCheckExpression env cond
  let reducedCondType = reduceType condType
  case reducedCondType of
    (FullNode meta' TBool) -> do
      thenBranchType <- typeCheckExpression env thenBranch
      elseBranchType <- typeCheckExpression env elseBranch
      let reducedThenType = reduceType thenBranchType
          reducedElseType = reduceType elseBranchType
      if reducedThenType == reducedElseType
        then pure thenBranchType
        else throwError'' meta' $ printf "TYPE ERROR: Type mismatch between then branch of type %s and else branch of type %s." (show thenBranchType) (show elseBranchType)
    _ -> throwError'' (getMetadata cond) $ printf "TYPE ERROR: Predicate of type %s needs to be a boolean in if-expression." (show condType)

-- TODO: We should kind check the return type in the type annotation to provide better error messages
typeCheckExpression env@TyperEnv{..} (FullNode meta (ETypeAbstraction label kind returnType body)) = do
  let newKindEnv = env { kindContext = Map.insert label kind kindContext
                       , aliasContext = Map.insert (extractName $ removeMetadata label) (FullNode meta (TVariable label)) aliasContext }
  resultType <- typeCheckExpression newKindEnv body
  case returnType of
    Just rt -> do potentialAlias <- findPlaceholderAlias env rt
                  annotatedReturnKind <- kindCheckWithEnvironment newKindEnv potentialAlias
                  case annotatedReturnKind of
                    (FullNode meta' StarK) -> do
                      let reducedAnnotatedType = reduceType potentialAlias
                          reducedResultType = reduceType resultType
                      if reducedAnnotatedType == reducedResultType
                      then pure . FullNode meta . TForall . FullNode meta' $ AbstractionInfo' label kind potentialAlias
                      else throwError'' meta $ printf "TYPE ERROR 2: Body type %s does not match annotated return type %s." (show potentialAlias) (show resultType)
                    other -> throwError'' meta $ printf "KIND ERROR: Annotated return type should have kind * but it has %s" (show other)
    Nothing -> pure . FullNode meta .  TForall . FullNode meta $ AbstractionInfo' label kind resultType

typeCheckExpression env (FullNode meta (ETypeApplication expr type')) = do
  reducedFunctionType <- reduceType <$> typeCheckExpression env expr
  case reducedFunctionType of
    (FullNode _ (TForall (FullNode meta' (AbstractionInfo' identifier kind bodyType)))) -> do
     potentialAlias <- findPlaceholderAlias env type'
     expectedKind <- kindCheckWithEnvironment env potentialAlias
     if kind == expectedKind
       then pure $ typeSubstitution identifier potentialAlias bodyType
       else throwError'' meta' $ printf "KIND ERROR: Expected kind %s for type application does not match with %s." (show expectedKind) (show kind)
    _ -> throwError'' meta $ printf "TYPE ERROR: Cannot do a type application with a value of type %s that is not a type abstraction." (show reducedFunctionType)
    
typeCheckExpression _ (FullNode meta (EOperation _ [])) = throwError'' meta "TYPE ERROR: Operators don't type check with no elements"

-- TODO: Check properly the arithmetic operators with the exhaustiveness -> THIS IS A TRAP
typeCheckExpression env (FullNode meta (EOperation operator list@(_:_))) = do
  operandsTypes <- for list (fmap reduceType . typeCheckExpression env)
  let checkIfAll type' = all (== type')
      x = head operandsTypes
      xs = tail operandsTypes
      checkIfList (FullNode _ (TList _)) = True
      checkIfList _ = False
      checkIfJust (FullNode _ (TList ((FullNode _ (TListInfo' (Just _)))))) = True
      checkIfJust _ = False

  case operator of
    (FullNode meta' OpAnd) -> if checkIfAll (FullNode meta' TBool) operandsTypes then pure (FullNode meta' TBool) else throwError'' meta' "TYPE ERROR: And operator asks for booleans."
    (FullNode meta' OpOr)  -> if checkIfAll (FullNode meta' TBool) operandsTypes then pure (FullNode meta' TBool) else throwError'' meta' "TYPE ERROR: Or operator asks for booleans."
    (FullNode meta' OpConcat) -> 
      case find (not . checkIfList) operandsTypes of
        Just different -> 
          throwError'' meta' $ printf "TYPE ERROR: Expected a List|_| but found '%s'" (show different)
        Nothing ->
          case filter checkIfJust operandsTypes of
            (y:ys) ->
              case find (/= y) ys of
                Just different -> 
                  throwError'' meta' $ printf "TYPE ERROR: Attempting to concat lists with distinct types. Received '%s' while expected 's'." (show different) (show y)
                Nothing -> pure y
            _ -> pure . FullNode meta' $ (TList . FullNode meta' . TListInfo') Nothing
    (FullNode meta' OpEqual) -> do
      case find (/= x) xs of
        Just firstDifferent -> throwError'' meta' $ printf "TYPE ERROR: Mismatch between elements at an equality comparison. Expected '%s' but got '%s'" (show x) (show firstDifferent)
        Nothing -> pure $ FullNode meta' TBool
    arithmetics -- Arithmetic
      | checkIfAll (makeEmptyNode TRational) operandsTypes -> pure $ if operator == makeEmptyNode OpLessThan then FullNode meta TBool else FullNode meta TRational
      | checkIfAll (makeEmptyNode TInteger) operandsTypes  -> pure $ if operator == makeEmptyNode OpLessThan then FullNode meta TBool else FullNode meta TInteger
      | otherwise -> throwError'' meta $ printf "TYPE ERROR: Arithmetic operator %s must use only numbers of the same sort." (show arithmetics)

kindCheckWithEnvironment :: TyperEnv -> Type -> ResultT Kind
kindCheckWithEnvironment env@TyperEnv{..} type' =
  case type' of
    (FullNode meta TUnit) -> pure $ FullNode meta StarK
    (FullNode meta TInteger) -> pure $ FullNode meta StarK
    (FullNode meta TRational) -> pure $ FullNode meta StarK
    (FullNode meta TBool) -> pure $ FullNode meta StarK
    (FullNode meta TString) -> pure $ FullNode meta StarK
    (FullNode _ (TAliasPlaceholder name)) ->
      let kind = Map.lookup (Name <$> name) kindContext in
      maybe (throwError'' (getMetadata name) $ printf "KIND ERROR: Unbound type alias %s in the environment." (show name)) return kind
    (FullNode _ (TAlias name _)) ->
      let kind = Map.lookup (Name <$> name) kindContext in
      maybe (throwError'' (getMetadata name) $ printf "KIND ERROR: Unbound type alias %s in the environment." (show name)) return kind
    (FullNode meta (TNominalRecord _ fields)) -> do
      internalKinds <- for (map snd fields) (kindCheckWithEnvironment env)
      if all (== makeEmptyNode StarK) internalKinds
      then pure $ FullNode meta StarK
      else throwError'' meta "KIND ERROR: Internal types of fields should have kind *."
    (FullNode meta (TAnonymousRecord fields)) -> do
      internalKinds <- for (map snd fields) (kindCheckWithEnvironment env)
      if all (== makeEmptyNode StarK) internalKinds
      then pure $ FullNode meta StarK
      else throwError'' meta "KIND ERROR: Internal types of fields should have kind *."
    (FullNode meta (TList (FullNode _ (TListInfo' Nothing)))) -> pure $ FullNode meta StarK
    (FullNode _ (TList (FullNode _ (TListInfo' (Just x))))) -> do
      internalKind <- kindCheckWithEnvironment env x
      case internalKind of
        (FullNode meta StarK) -> pure $ FullNode meta StarK
        (FullNode meta other) -> throwError'' meta $ printf "KIND ERROR: Internal types of lists should have kind * and this has %s" (show other)
    (FullNode meta (TVariable label)) ->
      let kind = Map.lookup label kindContext in
      maybe (throwError'' meta $ printf "KIND ERROR: Unbound type variable %s in the environment." (show label)) return kind
    (FullNode meta (TArrow input output)) -> do
      kindInput <- kindCheckWithEnvironment env input
      kindOutput <- kindCheckWithEnvironment env output
      case (kindInput, kindOutput) of
        (FullNode _ StarK, FullNode _ StarK) -> pure $ FullNode meta StarK
        (left, right) -> throwError'' meta $ printf "KIND ERROR: Expression arrow must have kind * -> * and it has %s -> %s." (show left) (show right)
    (FullNode meta' (TForall (FullNode _ (AbstractionInfo' identifier kind bodyType)))) -> do
       let newEnv = Map.insert identifier kind kindContext
       kindBody <- kindCheckWithEnvironment (env {kindContext = newEnv}) bodyType
       case kindBody of
        (FullNode meta StarK) -> pure $ FullNode meta StarK
        kind' -> throwError'' meta' $ printf "KIND ERROR: Foralls should return * but this has %s." (show kind')
    (FullNode _ (TApplication abstractionType argumentType)) -> do
      kindAbs <- kindCheckWithEnvironment env abstractionType
      kindArg <- kindCheckWithEnvironment env argumentType
      case kindAbs of
        (FullNode meta' (ArrowK k1 k2)) ->
          if k1 == kindArg
          then pure k2
          else throwError'' meta' $ printf "KIND ERROR: Kind argument %s does not match expected kind %s." (show kindArg) (show k1) 
        (FullNode meta' kind') -> throwError'' meta' $ printf "KIND ERROR: Type abstraction of kind %s is not a arrow kind" (show kind')
    (FullNode meta (TAbstraction (FullNode meta' (AbstractionInfo' label kind bodyType)))) -> do
      let newKindEnv = Map.insert label kind kindContext
          newAliasEnv = Map.insert (extractName $ removeMetadata label) (FullNode meta' (TVariable label)) aliasContext
      kindBody <- kindCheckWithEnvironment (env {kindContext = newKindEnv, aliasContext = newAliasEnv}) bodyType
      pure . FullNode meta $ ArrowK kind kindBody
    (FullNode meta (TAlgebraic typedNames)) -> do
      let types = map snd typedNames
      internalKinds <- for (concat types) (kindCheckWithEnvironment env)
      if all (== makeEmptyNode StarK) internalKinds
      then pure $ FullNode meta StarK
      else throwError'' meta "KIND ERROR: Internal types for ADTs of fields should have kind *."

reduceType :: Type -> Type
reduceType type' = 
  case type' of
    t@(FullNode _ TUnit) -> t
    t@(FullNode _ TInteger) -> t
    t@(FullNode _ TRational) -> t
    t@(FullNode _ TBool) -> t
    t@(FullNode _ TString) -> t
    (FullNode _ (TAliasPlaceholder _)) -> error "This should never happen. Something is broken in reduction xD"
    (FullNode _ (TAlias _ type'')) -> reduceType type''
    (FullNode meta (TNominalRecord name fields)) ->
      FullNode meta $ TNominalRecord name $ map (second reduceType) $ sortOn fst fields
    (FullNode meta (TAnonymousRecord fields)) ->
      FullNode meta $ TAnonymousRecord $ map (second reduceType) $ sortOn fst fields
    (FullNode meta (TAlgebraic namedTypes)) ->
      let (names, types) = unzip namedTypes
          reducedTypes = map (map reduceType) types
      in FullNode meta . TAlgebraic $ zip names reducedTypes
    (FullNode meta (TList (FullNode meta' (TListInfo' type'')))) -> FullNode meta . TList . FullNode meta' . TListInfo' $ fmap reduceType type''
    (FullNode meta (TArrow parameter returnType)) -> FullNode meta $ TArrow (reduceType parameter) (reduceType returnType)
    (FullNode meta (TVariable ident)) -> FullNode meta $ TVariable ident
    (FullNode meta (TForall (FullNode meta' (AbstractionInfo' parameterName parameterKind bodyType)))) ->
      let tForall = reduceType bodyType
      in FullNode meta $ TForall $ FullNode meta'
          (AbstractionInfo'
            parameterName
            parameterKind
            tForall)
    (FullNode meta (TAbstraction (FullNode meta' (AbstractionInfo' parameterName parameterKind body)))) -> 
      FullNode meta $ TAbstraction (FullNode meta' $ AbstractionInfo' parameterName parameterKind (reduceType body))
    (FullNode meta (TApplication function_ argument_)) -> 
      let function = reduceType function_
          argument = reduceType argument_
      in    
      case function of
        (FullNode _ (TAbstraction (FullNode _ (AbstractionInfo' parameterName _ body)))) -> reduceType $ typeSubstitution parameterName argument body
        _ -> FullNode meta $ TApplication function argument
