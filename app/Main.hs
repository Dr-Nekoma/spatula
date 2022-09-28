module Main (main) where

import Lib

type Env = [(String, Value)]

data Type_
    = T_Unit
    | T_Arrow Type_ Type_
    | T_Forall String Type_

data Expression
    = E_Unit
    | E_Variable String
    | E_Abstraction String Type_ Expression
    | E_Application Expression Expression
    | E_TypeAbstraction String Expression
    | E_TypeApplication Expression Type_

data Value
    = V_Unit
    | V_Closure String Type_ Expression Env
    | V_NativeFunction Value Value

main :: IO ()
main = someFunc
