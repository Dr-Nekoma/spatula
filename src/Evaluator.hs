{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
module Evaluator where

import Types
import qualified Data.Map as Map
import Data.Text ( unpack, Text, append, pack )
import Utils ( ResultT, throwError' )
import Text.Printf ( printf )
import Data.Traversable
import Control.Monad ( foldM )
import Data.List
import Data.Bifunctor ( Bifunctor(second) )
import Data.Maybe
import Control.Monad.IO.Class
import System.IO.Unsafe

type EvalEnv = Map.Map Text Value

newtype NativeFunction = NativeFunction (Value -> ResultT Value)

instance Eq NativeFunction where
    _ == _ = error "You can't compare native functions bro xD"

data Value
    = VUnit
    | VLiteral Literal
    | VClosure Text Expression EvalEnv
    | VNativeFunction NativeFunction
    | VList [Value]
    | VRecord [(Label, Value)]
    | VAlgebraic Label [Value]
    deriving Eq

instance Show Value where
  show VUnit = "()"
  show (VList []) = "'[]"
  show (VList (x:xs)) = foldl go ("'[" ++ show x) xs ++ "]"
    where go acc y = acc ++ " " ++ show y
  show (VLiteral literal) = show literal
  show VClosure {} = "<fun>" --printAlgebraic vclosure
  show (VNativeFunction _) = "<builtin>"
  show (VRecord []) = ""
  show (VRecord ((label, value):xs)) = "Label: " ++ unpack label ++ " - Value: " ++ show value ++ "\n" ++ show (VRecord xs)
  show (VAlgebraic name []) = "ADT " ++ unpack name
  show (VAlgebraic name list) = unpack name ++ " " ++ go list
    where go [] = "\n"
          go (x:xs) = show x ++ " " ++ go xs

generateAlgebraic :: Text -> Int -> Value
generateAlgebraic tag howMany = VClosure x (foldr (\e acc -> EAbstraction e TUnit Nothing acc) algebraicReturn xs) Map.empty
  where names@(x:xs) = map (pack . show) [1..howMany]
        algebraicReturn = EAlgebraic tag (map EVariable names)

addFunctionsToEnv2 :: EvalEnv -> Text -> Type -> EvalEnv
addFunctionsToEnv2 _ _ (TAlgebraic []) = error "This should be impossible. Great job Lemos with the Parser"
addFunctionsToEnv2 env typeName (TAlgebraic list) =
  let newEnv = foldr foldFields env list
      foldFields (name, types) acc =
        let generateValue name' [] = VAlgebraic name' []
            generateValue name' list' = generateAlgebraic name' (length list')
            y = generateValue name types
        in Map.insert (typeName <> "." <> name) y (Map.insert name y acc)
  in newEnv
addFunctionsToEnv2 env typeName (TAbstraction (AbstractionInfo _ _ type')) = addFunctionsToEnv2 env typeName type'
addFunctionsToEnv2 env _ _ = env

-- https://en.wikipedia.org/wiki/Fixed-point_combinator#Strict_fixed-point_combinator
internalZ :: Expression
internalZ = EAbstraction 
              "x"
              TUnit 
              Nothing 
              (EApplication 
                (EVariable "f") 
                (EAbstraction 
                  "v" 
                  TUnit
                  Nothing
                  (EApplication
                    (EApplication
                      (EVariable "x")
                      (EVariable "x"))
                    (EVariable "v"))))

evalDeclarations :: EvalEnv -> [Declaration] -> (Declaration -> Declaration) -> ResultT EvalEnv
evalDeclarations _ [] _ = throwError' "DECLARATION ERROR: No declaration found to evaluate "
evalDeclarations env list callback = foldM fun env list
  where fun acc (callback -> (DeclExpr expr)) = evalExpression acc expr >> return acc
        fun acc (callback -> (DeclVal name value)) =
          do v <- evalExpression acc value
             return $ Map.insert name v acc
        fun acc (callback -> (DeclType name t)) = do
          -- TODO add cases of a discriminated union to the context as functions         
          pure $ addFunctionsToEnv2 acc name t
        fun acc (callback -> (DeclFun name t expr)) = do
          let zCombinator = EAbstraction "f" t Nothing (EApplication internalZ internalZ)
              newExpr = EApplication zCombinator (EAbstraction name t Nothing expr)
          value <- evalExpression acc newExpr
          return $ Map.insert name value acc
        fun acc (callback -> (DeclModule name decls)) = do
          evalDeclarations acc decls (renameDeclaration (append name ":"))

evalExpression :: EvalEnv -> Expression -> ResultT Value

evalExpression env (EPatternMatching toMatch list) = do
  evaluatedMatch <- evalExpression env toMatch
  case evaluatedMatch of
    VAlgebraic label list' -> do
      case find (\(label', _, _) -> label' == label) list of
        Nothing -> throwError' $ printf "ERROR: Didn't find label %s in the list of labels" (show label)
        Just (_, binds, branch) -> do
             let addVariablesEnv vars values e = foldr (\(var, value) acc -> Map.insert var value acc) e $ zip vars values
             evalExpression (addVariablesEnv binds list' env) branch
    other -> throwError' $ printf "ERROR: Expected algebraic value but got %s" (show other)

evalExpression env (EAlgebraic label exprs) = do
  values <- for exprs (evalExpression env)
  pure $ VAlgebraic label values

-- TODO: add a sortBy so we can have record value equality
evalExpression env (EAnonymousRecord fields) = do
  let (names, exprs) = unzip fields
  values <- for exprs (evalExpression env)
  pure . VRecord $ zip names values

evalExpression env (EList list) = do
  evaluatedElems <- for list (evalExpression env)
  pure $ VList evaluatedElems

evalExpression env (EProgn list) = do
  evaluatedElems <- for list (evalExpression env)
  pure $ last evaluatedElems

evalExpression env (ERecordProjection expr label) = do
  potentialRecord <- evalExpression env expr
  case potentialRecord of
    VRecord fields -> do
      let fun target (name, _) = name == target
      case find (fun label) fields of
        Nothing ->  throwError' $ printf "ERROR: Record projection %s could not be found in %s" (unpack label) (show potentialRecord)
        Just (_, value) -> pure value
    other -> throwError' $ printf "ERROR: Record projection can only be used on records and got %s" (show other)

evalExpression env (ERecordUpdate expr toUpdateList) = do
  potentialRecord <- evalExpression env expr
  case potentialRecord of
    VRecord fields -> do
      toUpdateValues <- Map.fromList <$> for toUpdateList (mapM (evalExpression env))
      let mapFields = Map.fromList fields
      pure $ VRecord . Map.toList $ toUpdateValues `Map.union` mapFields
    other -> throwError' $ printf "TYPE ERROR: Record update can only be used on records and got %s" (show other)

evalExpression _ (ELiteral literal) = pure $ VLiteral literal

evalExpression env (EVariable label) =
  case Map.lookup label env of
    Nothing -> throwError' $ printf "ERROR: Unbound variable %s in the environment." (unpack label)
    Just var -> return var

evalExpression env (EAbstraction label _ _ body) =
  pure $ VClosure label body env

evalExpression env (EApplication fun arg) = do
  funValue <- evalExpression env fun
  argValue <- evalExpression env arg
  case funValue of
    VClosure label body closedEnv ->
      let newEnv = Map.insert label argValue closedEnv in
        evalExpression newEnv body
    VNativeFunction (NativeFunction natFun) ->
      natFun argValue
    other -> throwError' $ printf "ERROR: Attempted to apply value %s to %s that it is not a function." (show argValue) (show other)

evalExpression env (ECondition cond thenBranch elseBranch) = do
  test <- evalExpression env cond
  case test of
    VLiteral (LBool b) ->
      if b then
        evalExpression env thenBranch
      else
        evalExpression env elseBranch
    cond' -> throwError' $ printf "ERROR: The condition %s is not a bool." (show cond')

evalExpression env (ELet In bindings body) = do
  let (labels, expressions) = unzip bindings
  evaluatedExpressions <- for expressions (evalExpression env)
  let newEnv = foldl f env (zip labels evaluatedExpressions)
      f acc (label, expression) = Map.insert label expression acc
  evalExpression newEnv body

evalExpression env (ELet Plus [] body) = evalExpression env body
evalExpression env (ELet Plus ((label, expr):xs) body) = do
  evaluatedExpression <- evalExpression env expr
  evalExpression (Map.insert label evaluatedExpression env) (ELet Plus xs body)

evalExpression _ (EOperation OpAnd []) = return $ VLiteral (LBool True)
evalExpression env (EOperation OpAnd (x:xs)) = do
  operand <- evalExpression env x
  case operand of
    VLiteral (LBool False) -> return $ VLiteral (LBool False)
    VLiteral (LBool True) -> evalExpression env (EOperation OpAnd xs)
    _ -> throwError' "This should never happen"

-- TODO: Instead of relying on recursive calls of evalExpression, let's make an internal function and do the recursion there
evalExpression _ (EOperation OpOr []) = return $ VLiteral (LBool False)
evalExpression env (EOperation OpOr (x:xs)) = do
  operand <- evalExpression env x
  case operand of
    VLiteral (LBool True) -> return $ VLiteral (LBool True)
    VLiteral (LBool False) -> evalExpression env (EOperation OpOr xs)
    _ -> throwError' "This should never happen"

evalExpression _ (EOperation OpEqual []) = return $ VLiteral (LBool True)
evalExpression env (EOperation OpEqual (x:xs:rest)) = do
  operand1 <- evalExpression env x
  operand2 <- evalExpression env xs
  if operand1 == operand2
  then evalExpression env (EOperation OpEqual rest)
  else return $ VLiteral (LBool False)

evalExpression env (EOperation operator list@(_:_:_)) = do
  (x:xs) <- for list (evalExpression env)
  return $ foldl (operatorFunction operator) x xs

evalExpression _ (EOperation _ _) = throwError' "Open an issue about this: Operators are broken during evaluation"

evalExpression env (ETypeAbstraction _ _ _ body) =
  evalExpression env body

evalExpression env (ETypeApplication expr _) =
  evalExpression env expr

operatorFunction :: Operator -> Value -> Value -> Value
operatorFunction OpConcat (VList left) (VList right) = VList $ left ++ right
operatorFunction OpLessThan (VLiteral (LInteger element)) (VLiteral (LInteger acc)) = VLiteral . LBool $ element < acc
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

eval :: EvalEnv -> Expression -> ResultT Value
eval = evalExpression
