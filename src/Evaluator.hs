module Evaluator where

type Env = [(String, Value)]

data Type_
    = T_Unit
    | T_Integer
    | T_Rational
    | T_Bool
    | T_Arrow Type_ Type_
    | T_Forall String Type_
    | T_Variable String

data Literal
    = L_Unit
    | L_Integer Integer
    | L_Rational Rational
    | L_Bool Bool

data Expression
    = E_Literal Literal
    | E_Variable String
    | E_Abstraction String Type_ Expression
    | E_Application Expression Expression
    | E_TypeAbstraction String Expression
    | E_TypeApplication Expression Type_
    | E_V Value

data Value
    = V_Unit
    | V_Literal Literal
    | V_Closure String Type_ Expression Env
    | V_NativeFunction Value Value

eval :: Env -> Expression -> Either String Value


