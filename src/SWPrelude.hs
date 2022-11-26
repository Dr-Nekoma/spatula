{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
module SWPrelude ( evaluatorPrelude, typerPrelude ) where

import Types
import qualified Data.Map as Map
import Data.Text ( Text ) 
import Utils ( ResultT )
import Control.Monad.IO.Class (liftIO)
import Evaluator
import Data.Traversable ( for )

evaluatorPrelude :: Map.Map Text Value
evaluatorPrelude = Map.fromList $
                   map (fmap (VNativeFunction . NativeFunction))
                     [ ("print", ourPrint),
                       ("car", car),
                       ("cdr", cdr),
                       ("map", map'),
                       ("filter", filter')]
  
typerPrelude :: Map.Map Text Type
typerPrelude = Map.fromList list
    where list = [("print", TForall $ TForallInfo "T" StarK (TArrow (TVariable "T") TUnit)),
                  ("car", TForall $ TForallInfo "T" StarK (TArrow (TList . Just $ TVariable "T") (TVariable "T"))),
                  ("cdr", TForall $ TForallInfo "T" StarK (TArrow (TList . Just $ TVariable "T") (TList . Just $ TVariable "T"))),
                  ("map", mapType),
                  ("filter", filterType)]

mapType :: Type
mapType =
  TForall $ TForallInfo "A" StarK
  (TForall $ TForallInfo "B" StarK
   (TArrow (TArrow (TVariable "A") (TVariable "B")) (TArrow (TList . Just $ TVariable "A") (TList . Just $ TVariable "B"))))

filterType :: Type
filterType =
  TForall $ TForallInfo "A" StarK
   (TArrow (TArrow (TVariable "A") TBool) (TArrow (TList . Just $ TVariable "A") (TList . Just $ TVariable "A")))

car :: Value -> ResultT Value
car (VList []) = fail "Can't apply 'car' function in empty lists"
car (VList list) = return . head $ list
car _ = fail "Function 'car' can only be applied to lists"

cdr :: Value -> ResultT Value
cdr (VList []) = fail "Can't apply 'cdr' to an empty list"
cdr (VList list) = return . VList . tail $ list
cdr _ = fail "Function 'car' can only be applied to lists"

map' :: Value -> ResultT Value
map' = \value ->
         case value of
           VClosure label body env ->
             return $ VNativeFunction . NativeFunction $
               \list -> case list of
                          VList list' -> VList <$> for list' (\a -> eval (Map.insert label a env) body)
                          _ -> fail "Expecting a list as an argument for the map function"
           _ -> fail "Expecting a closure for the map function"

filter' :: Value -> ResultT Value
filter' = \value ->
         case value of
           VClosure label body env ->
             return $ VNativeFunction . NativeFunction $
               \list -> case list of
                          VList list' -> VList . (map fst) . (filter (\(_,a) -> a == VLiteral (LBool True))) . (zip list') <$> for list' (\a -> eval (Map.insert label a env) body)
                          _ -> fail "Expecting a list as an argument for the filter function"
           _ -> fail "Expecting a closure for the filter function"
           
ourPrint :: Value -> ResultT Value
ourPrint value = do
    liftIO $ print (show value)
    return VUnit
