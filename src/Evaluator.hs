{-# LANGUAGE OverloadedStrings #-}
module Evaluator ( eval ) where

import Types ( Expression(..), Literal(LBool) )
import qualified Data.Map as Map
import Data.Text ( Text )

data Value
    = VUnit
    | VLiteral Literal
    | VClosure Text Expression EvalEnv
    | VNativeFunction (Value -> EvalResult)

instance Show Value where
  show VUnit = "()"
  show (VLiteral literal) = show literal
  show (VClosure {}) = "<fun>"
  show (VNativeFunction _) = "<native>"

type EvalResult = Either Text Value
type EvalEnv = Map.Map Text Value

eval :: EvalEnv -> Expression -> EvalResult

eval _ (ELiteral literal) = pure $ VLiteral literal

eval env (EVariable label) =
  case Map.lookup label env of
    Nothing -> Left "Couldn't find your variable in the environment"
    Just value -> pure value

eval env (EAbstraction label _ _ body) =
  pure $ VClosure label body env

eval env (EApplication fun arg) = do
  funValue <- eval env fun
  argValue <- eval env arg
  case funValue of
    VClosure label body closedEnv ->
      let newEnv = Map.insert label argValue closedEnv in
        eval newEnv body
    VNativeFunction natFun ->
      natFun argValue
    _ -> Left "Failed attempting to apply a value to something that is not a function."

eval env (ECondition cond thenBranch elseBranch) = do
  test <- eval env cond
  case test of
    VLiteral (LBool b) ->
      if b then
        eval env thenBranch
      else
        eval env elseBranch
    _ -> Left "Condition expression could not be evaluated to a boolean."
  
eval env (ETypeAbstraction _ body) =
  eval env body

eval env (ETypeApplication expr _) =
  eval env expr
