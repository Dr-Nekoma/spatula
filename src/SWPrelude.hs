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

arithmeticNative :: (Integer -> Integer -> Integer) -> Text -> Value -> ResultT Value
arithmeticNative op name (VLiteral literal) = 
    case literal of
        LInteger first -> return $ VNativeFunction $ \(VLiteral (LInteger second)) -> return . VLiteral . LInteger $ op first second
        _ -> fail "error"
arithmeticNative _ name _ = fail $ printf "Can't operate %s on non-numbers" (show $ unpack name)

auxiliary :: Value -> ResultT Value
auxiliary value = do
    liftIO $ print (show value)
    return VUnit
