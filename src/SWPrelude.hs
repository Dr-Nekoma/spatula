{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
module SWPrelude where

import Types
import Data.Text
import qualified Data.Map as Map
import Data.Text ( Text, unpack, pack )
import Utils ( ResultT )
import Text.Printf ( printf )
import Control.Monad.IO.Class (liftIO)

type EvalEnv = Map.Map Text Value

data Value
    = VUnit
    | VLiteral Literal
    | VClosure Text Expression EvalEnv
    | VNativeFunction (Value -> ResultT Value)

instance Show Value where
  show VUnit = "()"
  show (VLiteral literal) = show literal
  show (VClosure {}) = "<fun>"
  show (VNativeFunction _) = "<builtin>"

evaluatorPrelude :: Map.Map Text Value
evaluatorPrelude = Map.fromList [("show", VNativeFunction ourPrint)]

typerPrelude :: Map.Map Text Type
typerPrelude = Map.fromList list
    where list = [("show", TArrow TInteger TUnit)]--TForall $ TForallInfo "T" StarK (TArrow (TVariable "T") TUnit))]

ourPrint :: Value -> ResultT Value
ourPrint value = do
    liftIO $ print (show value)
    return VUnit
