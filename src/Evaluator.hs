module Evaluator where

type Env = [(String, Value)]

data Type
    = TUnit
    | TInteger
    | TRational
    | TBool
    | TArrow Type Type
    | TForall String Type
    | TVariable String

data Literal
    = LUnit
    | LInteger Integer
    | LRational Rational
    | LBool Bool

data Expression
    = ELiteral Literal
    | EVariable String
    | EAbstraction String Type Expression
    | EApplication Expression Expression
    | ETypeAbstraction String Expression
    | ETypeApplication Expression Type
    | EV Value

data Value
    = VUnit
    | VLiteral Literal
    | VClosure String Type Expression Env
    | VNativeFunction Value Value

eval :: Env -> Expression -> Either String Value
eval = undefined

