{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}
module Evaluator where

import Types
import qualified Data.Map as Map
import Data.Text ( unpack, Text, append, pack )
import Utils ( ResultT, throwError', throwError'' )
import Text.Printf ( printf )
import Data.Traversable
import Control.Monad ( foldM )
import Data.List
import Data.Bifunctor ( Bifunctor(first, second) )
import Data.Either
import Data.Maybe
import Control.Monad
import Control.Monad.IO.Class
import Parser
import System.IO.Unsafe
import Data.List.Extra (firstJust)
import Text.Parsec (parse)

type EvalEnv = Map.Map Text Value

newtype NativeFunction = NativeFunction (Value -> ResultT Value)

instance Eq NativeFunction where
    _ == _ = error "You can't compare native functions bro xD"

type Value = FullNode Value'

data Value'
    = VUnit
    | VLiteral Literal'
    | VClosure Label Expression EvalEnv
    | VNativeFunction NativeFunction
    | VList [Value]
    | VRecord [(Label, Value)]
    | VAlgebraic Label [Value]
    deriving Eq

instance Show Value' where
  show VUnit = "()"
  show (VList []) = "'[]"
  show (VList (x:xs)) = foldl go ("'[" ++ show x) xs ++ "]"
    where go acc y = acc ++ " " ++ show y
  show (VLiteral literal) = show literal
  show VClosure {} = "<fun>" --printAlgebraic vclosure
  show (VNativeFunction _) = "<builtin>"
  show (VRecord []) = ""
  show (VRecord ((label, value):xs)) = "Label: " ++ unpack (removeMetadata label) ++ " - Value: " ++ show value ++ "\n" ++ show (VRecord xs)
  show (VAlgebraic name []) = "ADT " ++ unpack (removeMetadata name)
  show (VAlgebraic name list) = unpack (removeMetadata name) ++ " " ++ go list
    where go [] = "\n"
          go (x:xs) = show x ++ " " ++ go xs

generateAlgebraic :: Text -> Int -> Value'
generateAlgebraic tag howMany = VClosure (makeEmptyNode x) (foldr (\e acc -> makeEmptyNode $ EAbstraction (makeEmptyNode e) unitT Nothing acc) algebraicReturn xs) Map.empty
  where names@(x:xs) = map (pack . show) [1..howMany]
        unitT = makeEmptyNode TUnit
        names' = map makeEmptyNode names
        algebraicReturn = makeEmptyNode $ EAlgebraic (makeEmptyNode tag) (map (makeEmptyNode . EVariable) names')

addFunctionsToEnv2 :: EvalEnv -> Text -> Type -> EvalEnv
addFunctionsToEnv2 _ _ (FullNode _ (TAlgebraic [])) = error "This should be impossible. Great job Lemos with the Parser"
addFunctionsToEnv2 env typeName (FullNode _ (TAlgebraic list)) =
  let newEnv = foldr foldFields env list
      foldFields (name, types) acc =
        let generateValue name'@(FullNode meta _) [] = FullNode meta $ VAlgebraic name' []
            generateValue (FullNode meta name') list' = FullNode meta $ generateAlgebraic name' (length list')
            y = generateValue name types
        in Map.insert (typeName <> "." <> removeMetadata name) y (Map.insert (removeMetadata name) y acc)
  in newEnv
addFunctionsToEnv2 env typeName (FullNode _ (TAbstraction (FullNode _ (AbstractionInfo' _ _ type')))) = addFunctionsToEnv2 env typeName type'
addFunctionsToEnv2 env _ _ = env

-- https://en.wikipedia.org/wiki/Fixed-point_combinator#Strict_fixed-point_combinator
internalZ :: Expression
internalZ = makeEmptyNode (EAbstraction 
              (makeEmptyNode "x")
              (makeEmptyNode TUnit)
              Nothing 
              (makeEmptyNode (EApplication 
                (makeEmptyNode (EVariable $ makeEmptyNode "f")) 
                (makeEmptyNode (EAbstraction 
                  (makeEmptyNode  "v")
                  (makeEmptyNode TUnit)
                  Nothing
                  (makeEmptyNode (EApplication
                    (makeEmptyNode (EApplication (makeEmptyNode (EVariable $ makeEmptyNode "x")) (makeEmptyNode (EVariable $ makeEmptyNode "x"))))
                     (makeEmptyNode (EVariable $ makeEmptyNode "v")))))))))

loadFile2 :: EvalEnv -> FullNode FilePath -> ResultT (Either Value EvalEnv)
loadFile2 env metaFilepath = do
  let meta = getMetadata metaFilepath
      filepath = removeMetadata metaFilepath
  result <- liftIO $ customParse fileP filepath
  case result of
    Left errorParse -> throwError'' meta $ printf "DECLARATION ERROR: Could not load file. Error % was found" (show errorParse)
    Right (FullNode _ decls) -> Right <$> foldM (\acc decl -> fromRight acc <$> evalDeclaration acc decl) env decls

evalDeclarations :: EvalEnv -> [Declaration] -> ResultT EvalEnv
evalDeclarations env [] = pure env
evalDeclarations env list = foldM fun env list
  where fun acc decl = fromRight acc <$> evalDeclaration acc decl

evalDeclaration :: EvalEnv -> Declaration -> ResultT (Either Value EvalEnv)
evalDeclaration env (FullNode _ (DeclExpr expr)) = Left <$> evalExpression env expr
evalDeclaration env (FullNode _ (DeclVal name value)) = Right . (flip $ Map.insert (removeMetadata name)) env <$> evalExpression env value
evalDeclaration env (FullNode _ (DeclLoad filepath)) = loadFile2 env filepath
evalDeclaration env (FullNode _ (DeclType name t)) = pure . Right $ addFunctionsToEnv2 env (removeMetadata name) t
evalDeclaration env (FullNode _ (DeclFun name t expr)) = do
          let zCombinator = makeEmptyNode $ EAbstraction (makeEmptyNode "f") t Nothing (makeEmptyNode (EApplication internalZ internalZ))
              newExpr = makeEmptyNode $ EApplication zCombinator (makeEmptyNode (EAbstraction name t Nothing expr))
          value <- evalExpression env newExpr
          return . Right $ Map.insert (removeMetadata name) value env
evalDeclaration _ (FullNode _ (DeclModule _ _)) = error "TODO: Can't eval module declaration"

createBinds2 :: Value -> Pattern -> Maybe [(Label, Value)]
createBinds2 _ (FullNode _ PWildcard) = Just []
createBinds2 value (FullNode _ (PVariable label)) = Just [(label, value)]
createBinds2 value (FullNode _ (PAs pattern' label)) = (:) (label, value) <$> createBinds2 value pattern'
createBinds2 value (FullNode _ (PDisjunctive firstPattern secondPattern)) = firstJust (createBinds2 value) [firstPattern, secondPattern]
createBinds2 (FullNode _ (VAlgebraic identifier values)) (FullNode  _  (PSumType constructor patterns)) =
  if identifier == constructor
  then concat <$> zipWithM createBinds2 values patterns
  else Nothing
createBinds2 (FullNode _ VUnit) (FullNode _ (PLiteral LUnit)) = Just []
createBinds2 (FullNode _ (VLiteral value')) (FullNode _ (PLiteral value)) = if value == value' then Just [] else Nothing
createBinds2 _ _ = Nothing

evalExpression :: EvalEnv -> Expression -> ResultT Value

evalExpression _ (FullNode _ (EPatternMatching _ [])) = error "This should never happen. Good job with the parsing Lemos xD."

evalExpression env (FullNode meta (EPatternMatching toMatch list)) = do
  evaluatedMatch <- evalExpression env toMatch
  let folder Nothing (pattern', guard', body) = do
        case createBinds2 evaluatedMatch pattern' of
          Nothing -> pure Nothing
          Just (map (first removeMetadata) -> binds) -> do 
            let newEnv = Map.union (Map.fromList binds) env
            case guard' of
              Nothing -> Just <$> evalExpression newEnv body
              Just g -> do
                evaluatedGuard <- evalExpression newEnv g
                case removeMetadata evaluatedGuard of
                  VLiteral (LBool True) -> Just <$> evalExpression newEnv body
                  _ -> pure Nothing
      folder expr _ = pure expr
  expr <- foldM folder Nothing list
  maybe (throwError'' meta $ printf "ERROR: Could not evaluate %s in pattern match" (show toMatch)) pure expr

evalExpression env (FullNode meta (EAlgebraic label exprs)) = do
  values <- for exprs (evalExpression env)
  pure . FullNode meta $ VAlgebraic label values

evalExpression env (FullNode meta (ENominalRecord _ fields)) = do
  let (names, exprs) = unzip fields
  values <- for exprs (evalExpression env)
  pure . FullNode meta . VRecord $ sortOn fst $ zip names values

evalExpression env (FullNode meta (EAnonymousRecord fields)) = do
  let (names, exprs) = unzip fields
  values <- for exprs (evalExpression env)
  pure . FullNode meta . VRecord $ sortOn fst $ zip names values

evalExpression env (FullNode meta (EList list)) = do
  evaluatedElems <- for list (evalExpression env)
  pure . FullNode meta $ VList evaluatedElems

evalExpression env (FullNode _ (EProgn list)) = do
  evaluatedElems <- for list (evalExpression env)
  pure $ last evaluatedElems

evalExpression env (FullNode meta (ERecordProjection expr label)) = do
  potentialRecord <- evalExpression env expr
  case potentialRecord of
    (FullNode meta' (VRecord fields)) -> do
      let fun target (name, _) = name == target
      case find (fun label) fields of
        Nothing ->  throwError'' meta' $ printf "ERROR: Record projection %s could not be found in %s" (unpack $ removeMetadata label) (show potentialRecord)
        Just (_, value) -> pure value
    other -> throwError'' meta $ printf "ERROR: Record projection can only be used on records and got %s" (show other)

evalExpression env (FullNode meta (ERecordUpdate expr toUpdateList)) = do
  potentialRecord <- evalExpression env expr
  case potentialRecord of
    (FullNode meta' (VRecord fields)) -> do
      toUpdateValues <- Map.fromList <$> for toUpdateList (mapM (evalExpression env))
      let mapFields = Map.fromList fields
      pure . FullNode meta' . VRecord $ Map.toList $ toUpdateValues `Map.union` mapFields
    other -> throwError'' meta $ printf "TYPE ERROR: Record update can only be used on records and got %s" (show other)

evalExpression _ (FullNode meta (ELiteral literal)) = pure . FullNode meta $ VLiteral literal

evalExpression env (FullNode meta (EVariable label)) =
  let label' = removeMetadata label in
  case Map.lookup label' env of
    Nothing -> throwError'' meta $ printf "ERROR: Unbound variable %s in the environment." (unpack label')
    Just var -> return var

evalExpression env (FullNode meta (EAbstraction label _ _ body)) =
  pure . FullNode meta $ VClosure label body env

evalExpression env (FullNode meta (EApplication fun arg)) = do
  funValue <- evalExpression env fun
  argValue <- evalExpression env arg
  case funValue of
    (FullNode _ (VClosure label body closedEnv)) ->
      let newEnv = Map.insert (removeMetadata label) argValue closedEnv in
        evalExpression newEnv body
    (FullNode _ (VNativeFunction (NativeFunction natFun))) ->
      natFun argValue
    other -> throwError'' meta $ printf "ERROR: Attempted to apply value %s to %s that it is not a function." (show argValue) (show other)

evalExpression env (FullNode meta (ECondition cond thenBranch elseBranch)) = do
  test <- evalExpression env cond
  case test of
    (FullNode _ (VLiteral (LBool b))) ->
      if b then
        evalExpression env thenBranch
      else
        evalExpression env elseBranch
    cond' -> throwError'' meta $ printf "ERROR: The condition %s is not a bool." (show cond')

evalExpression env (FullNode _ (ELet (removeMetadata -> In) bindings body)) = do
  let (map removeMetadata -> labels, expressions) = unzip bindings
  evaluatedExpressions <- for expressions (evalExpression env)
  let newEnv = foldl f env (zip labels evaluatedExpressions)
      f acc (label, expression) = Map.insert label expression acc
  evalExpression newEnv body

evalExpression env (FullNode _ (ELet (removeMetadata -> Plus) [] body)) = evalExpression env body
evalExpression env (FullNode meta (ELet p@(FullNode _ Plus) ((label, expr):xs) body)) = do
  evaluatedExpression <- evalExpression env expr
  evalExpression (Map.insert (removeMetadata label) evaluatedExpression env) (FullNode meta $ ELet p xs body)

evalExpression _ (FullNode meta (EOperation (FullNode _ OpAnd) [])) = return . FullNode meta . VLiteral $ LBool True
evalExpression env (FullNode _ (EOperation (FullNode _ OpAnd) (x:xs))) = do
  operand <- evalExpression env x
  case operand of
    (FullNode meta (VLiteral (LBool False))) -> pure . FullNode meta . VLiteral $ LBool False
    (FullNode meta (VLiteral (LBool True))) -> evalExpression env (FullNode meta $ EOperation (FullNode meta OpAnd) xs)
    _ -> error "This should never happen"

-- TODO: Instead of relying on recursive calls of evalExpression, let's make an internal function and do the recursion there
evalExpression _ (FullNode meta (EOperation (FullNode _ OpOr) [])) = return . FullNode meta . VLiteral $ LBool False
evalExpression env (FullNode _ (EOperation (FullNode _ OpOr) (x:xs))) = do
  operand <- evalExpression env x
  case operand of
    (FullNode meta (VLiteral (LBool True)))  -> pure . FullNode meta . VLiteral $ LBool True
    (FullNode meta (VLiteral (LBool False))) -> evalExpression env (FullNode meta $ EOperation (FullNode meta OpOr) xs)
    _ -> error "This should never happen"

evalExpression _ (FullNode meta (EOperation (FullNode _ OpEqual) [])) = return . FullNode meta . VLiteral $ LBool True
evalExpression env (FullNode meta (EOperation (FullNode meta' OpEqual) (x:xs:rest))) = do
  operand1 <- evalExpression env x
  operand2 <- evalExpression env xs
  if operand1 == operand2
  then evalExpression env $ FullNode meta (EOperation (FullNode meta' OpEqual) rest)
  else return . FullNode meta . VLiteral $ LBool False

evalExpression env (FullNode meta (EOperation operator list@(_:_:_))) = do
  (x':xs') <- for list (evalExpression env)
  let x = removeMetadata x'
      xs = map removeMetadata xs'
  return . FullNode meta $ foldl (operatorFunction (removeMetadata operator)) x xs

evalExpression _ (FullNode _ (EOperation _ _)) = error "Open an issue about this: Operators are broken during evaluation"

evalExpression env (FullNode _ (ETypeAbstraction _ _ _ body)) =
  evalExpression env body

evalExpression env (FullNode _ (ETypeApplication expr _)) =
  evalExpression env expr

operatorFunction :: Operator' -> Value' -> Value' -> Value'
operatorFunction OpConcat (VList left) (VList right) = VList $ left ++ right
operatorFunction OpLessThan (VLiteral (LInteger element)) (VLiteral (LInteger acc)) = VLiteral .  LBool $ element < acc
operatorFunction OpLessThan (VLiteral (LRational element)) (VLiteral (LRational acc)) = VLiteral . LBool $ element < acc
operatorFunction OpPlus (VLiteral (LInteger element)) (VLiteral (LInteger acc)) = VLiteral . LInteger $ element + acc
operatorFunction OpPlus (VLiteral (LRational element)) (VLiteral (LRational acc)) = VLiteral . LRational $ element + acc
operatorFunction OpMul (VLiteral (LInteger element)) (VLiteral (LInteger acc)) = VLiteral . LInteger $ element * acc
operatorFunction OpMul (VLiteral (LRational element)) (VLiteral (LRational acc)) = VLiteral . LRational $ element * acc
operatorFunction OpDiv (VLiteral (LInteger element)) (VLiteral (LInteger acc)) = VLiteral . LInteger $ div element acc
operatorFunction OpDiv (VLiteral (LRational element)) (VLiteral (LRational acc)) = VLiteral . LRational $ element / acc
operatorFunction OpMinus (VLiteral (LInteger element)) (VLiteral (LInteger acc)) = VLiteral . LInteger $ element - acc
operatorFunction OpMinus (VLiteral (LRational element)) (VLiteral (LRational acc)) = VLiteral . LRational $ element - acc
operatorFunction op element acc = error $ printf "Error in fold of %s with element %s and accumulator %s" (show op) (show element) (show acc)
