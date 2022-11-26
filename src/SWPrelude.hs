{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
module SWPrelude ( EvalEnv, Value(..), NativeFunction(..), evaluatorPrelude, typerPrelude ) where

import Types
import qualified Data.Map as Map
import Data.Text ( Text ) 
import Utils ( ResultT )
import Control.Monad.IO.Class (liftIO)

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
    deriving Eq

instance Show Value where
  show VUnit = "()"
  show (VList list) = show list
  show (VLiteral literal) = show literal
  show (VClosure {}) = "<fun>"
  show (VNativeFunction _) = "<builtin>"

evaluatorPrelude :: Map.Map Text Value
evaluatorPrelude = Map.fromList [("print", VNativeFunction $ NativeFunction ourPrint),
                                 ("car", VNativeFunction $ NativeFunction car),
                                 ("cdr", VNativeFunction $ NativeFunction cdr)]

typerPrelude :: Map.Map Text Type
typerPrelude = Map.fromList list
    where list = [("print", TForall $ TForallInfo "T" StarK (TArrow (TVariable "T") TUnit)),
                  ("car", TForall $ TForallInfo "T" StarK (TArrow (TList . Just $ TVariable "T") (TVariable "T"))),
                  ("cdr", TForall $ TForallInfo "T" StarK (TArrow (TList . Just $ TVariable "T") (TList . Just $ TVariable "T")))]

car :: Value -> ResultT Value
car (VList []) = fail "Can't apply 'car' function in empty lists"
car (VList list) = return . head $ list
car _ = fail "Function 'car' can only be applied to lists"

cdr :: Value -> ResultT Value
cdr (VList []) = fail "Can't apply 'cdr' to an empty list"
cdr (VList list) = return . VList . tail $ list
cdr _ = fail "Function 'car' can only be applied to lists"



ourPrint :: Value -> ResultT Value
ourPrint value = do
    liftIO $ print (show value)
    return VUnit
