{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
module SWPrelude where

import Types
import Data.Text
import qualified Data.Map as Map
import Data.Text ( Text, unpack, pack )
import Utils ( Result )
import Text.Printf ( printf )
import System.IO.Unsafe

type EvalEnv = Map.Map Text Value

data Value
    = VUnit
    | VLiteral Literal
    | VClosure Text Expression EvalEnv
    | VNativeFunction (Value -> Result Value)

instance Show Value where
  show VUnit = "()"
  show (VLiteral literal) = show literal
  show (VClosure {}) = "<fun>"
  show (VNativeFunction _) = "<builtin>"

evaluatorPrelude :: Map.Map Text Value
evaluatorPrelude = Map.fromList list
    where list = [
            ("+", VNativeFunction (arithmeticNative (+) "+")),
            ("-", VNativeFunction (arithmeticNative (-) "-")),
            ("*", VNativeFunction (arithmeticNative (*) "*")),
            ("show", VNativeFunction auxiliary)]

typerPrelude :: Map.Map Text Type
typerPrelude = Map.fromList list
    where list = [
            ("+", TArrow TInteger (TArrow TInteger TInteger)),
            ("*", TArrow TInteger (TArrow TInteger TInteger)),
            ("-", TArrow TInteger (TArrow TInteger TInteger)),
            ("show", TArrow TInteger TUnit)]--TForall $ TForallInfo "T" StarK (TArrow (TVariable "T") TUnit))]

kinderPrelude :: Map.Map Text Kind
kinderPrelude = Map.fromList list
    where list = [
            ("+", StarK),
            ("-", StarK),
            ("*", StarK),
            ("show", StarK)]

arithmeticNative :: (Integer -> Integer -> Integer) -> Text -> Value -> Result Value
arithmeticNative op name (VLiteral literal) = 
    case literal of
        LInteger first -> Right $ VNativeFunction $ \(VLiteral (LInteger second)) -> Right . VLiteral . LInteger $ op first second
        _ -> Left "error"
arithmeticNative _ name _ = Left $ pack $ printf "Can't operate %s on non-numbers" (show $ unpack name)

auxiliary :: Value -> Result Value
auxiliary value = do
    let !a = unsafePerformIO (print (show value))
    return VUnit
