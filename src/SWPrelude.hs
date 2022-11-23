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
    deriving Eq

instance Show Value where
  show VUnit = "()"
  show (VLiteral literal) = show literal
  show (VClosure {}) = "<fun>"
  show (VNativeFunction _) = "<builtin>"

evaluatorPrelude :: Map.Map Text Value
evaluatorPrelude = Map.fromList [("show", VNativeFunction $ NativeFunction ourPrint)]

typerPrelude :: Map.Map Text Type
typerPrelude = Map.fromList list
    where list = [("show", TForall $ TForallInfo "T" StarK (TArrow (TVariable "T") TUnit))]

ourPrint :: Value -> ResultT Value
ourPrint value = do
    liftIO $ print (show value)
    return VUnit
