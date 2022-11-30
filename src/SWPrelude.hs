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
                       ("filter", filter'),
                       ("fold", fold'),
                       ("foldBack", foldBack)]
  
typerPrelude :: Map.Map Text Type
typerPrelude = Map.fromList list
    where list = [("print", TForall $ AbstractionInfo "T" StarK (TArrow (TVariable "T") TUnit)),
                  ("car", TForall $ AbstractionInfo "T" StarK (TArrow (TList . TListInfo . Just $ TVariable "T") (TVariable "T"))),
                  ("cdr", TForall $ AbstractionInfo "T" StarK (TArrow (TList . TListInfo . Just $ TVariable "T") (TList . TListInfo . Just $ TVariable "T"))),
                  ("map", mapType),
                  ("filter", filterType),
                  ("fold", foldType),
                  ("foldBack", foldType)]

mapType :: Type
mapType =
  TForall $ AbstractionInfo "A" StarK
  (TForall $ AbstractionInfo "B" StarK
   (TArrow (TArrow (TVariable "A") (TVariable "B")) (TArrow (TList . TListInfo . Just $ TVariable "A") (TList . TListInfo . Just $ TVariable "B"))))

filterType :: Type
filterType =
  TForall $ AbstractionInfo "A" StarK
   (TArrow (TArrow (TVariable "A") TBool) (TArrow (TList . TListInfo . Just $ TVariable "A") (TList . TListInfo . Just $ TVariable "A")))

foldType :: Type
foldType =
  TForall $ AbstractionInfo "A" StarK
  (TForall $ AbstractionInfo "B" StarK
   (TArrow (TArrow (TVariable "A") (TArrow (TVariable "B") (TVariable "B"))) 
     (TArrow (TVariable "B") (TArrow (TList . TListInfo . Just $ TVariable "A") (TVariable "B")))))

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
           
-- (a -> b -> b) -> b -> t a -> b

fold' :: Value -> ResultT Value
fold' = undefined
-- fold' = \value ->
--          case value of
--            VClosure label body env ->
--              return $ VNativeFunction . NativeFunction $
--                \acc -> return $ VNativeFunction . NativeFunction $
--                         \list -> case list of
--                           VList list' -> do
--                             let fun element = do
--                                   partialFunction <- runExceptT $ eval (Map.insert label a env) body
--                                     case partialFunction of
--                                       Left error -> fail error
--                                       Right function -> 
--                                         case function of
--                                           VClosure label' body' env' -> eval (Map.insert label' acc env') body'            
--                                           _ -> fail "Expected a closure in the fold found something else"
--                             for list' fun
--                           _ -> fail "Expecting a list as an argument for the filter function" 
--            _ -> fail "Expecting a closure for the fold function"

foldBack :: Value -> ResultT Value
foldBack = undefined

ourPrint :: Value -> ResultT Value
ourPrint value = do
    liftIO $ print (show value)
    return VUnit
