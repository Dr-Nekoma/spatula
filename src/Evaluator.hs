{-# LANGUAGE OverloadedStrings #-}
module Evaluator ( evalDeclarations, eval, Value(..), NativeFunction(..), EvalEnv, evalExpression ) where

import Types
import qualified Data.Map as Map
import Data.Text ( unpack, Text )
import Utils ( ResultT, throwError' )
import Text.Printf ( printf )
import Data.Traversable
import Control.Monad ( foldM )

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
    deriving Eq

instance Show Value where
  show VUnit = "()"
  show (VList []) = "'[]"
  show (VList (x:xs)) = foldl go ("'[" ++ show x) xs ++ "]"
    where go acc y = acc ++ " " ++ show y
  show (VLiteral literal) = show literal
  show (VClosure {}) = "<fun>"
  show (VNativeFunction _) = "<builtin>"
  show (VRecord []) = ""
  show (VRecord ((label, value):xs)) = "Label: " ++ unpack label ++ " - Value: " ++ show value ++ "\n" ++ show (VRecord xs)

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

evalDeclarations :: EvalEnv -> [Declaration] -> ResultT EvalEnv
evalDeclarations _ [] = throwError' "DECLARATION ERROR: No declaration found to evaluate "
evalDeclarations env list = foldM fun env list
  where fun acc (DeclExpr expr) = evalExpression acc expr >> return acc
        fun acc (DeclVal name value) =
          do value <- evalExpression acc value
             return $ Map.insert name value acc
        fun acc (DeclFun name t expr) = do
          let zCombinator = EAbstraction "f" t Nothing (EApplication internalZ internalZ)
              newExpr = EApplication zCombinator (EAbstraction name t Nothing expr)
          value <- evalExpression acc newExpr
          return $ Map.insert name value acc

evalExpression :: EvalEnv -> Expression -> ResultT Value


-- TODO: add a sortBy so we can have record value equality
evalExpression env (EAnonymusRecord fields) = do
  let (names, exprs) = unzip fields
  values <- for exprs (evalExpression env)
  pure . VRecord $ zip names values

evalExpression env (EList list) = do
  evaluatedElems <- for list (evalExpression env)
  pure $ VList evaluatedElems

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
