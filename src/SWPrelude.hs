{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module SWPrelude ( evaluatorPrelude, typerPrelude ) where

import Types
import qualified Data.Map as Map
import Data.Text ( Text ) 
import Utils ( ResultT )
import Control.Monad.IO.Class (liftIO)
import Evaluator
import Data.Traversable ( for )
import Control.Monad ( foldM )

evaluatorPrelude :: Map.Map Text Value
evaluatorPrelude = Map.fromList $
                   map (fmap (VNativeFunction . NativeFunction))
                     [ ("print", ourPrint),
                       ("car", car),
                       ("cdr", cdr),
                       ("map", map'),
                       ("filter", filter'),
                       ("fold", fold' id),
                       ("foldBack", fold' reverse)]
  
typerPrelude :: Map.Map Text Type
typerPrelude = Map.fromList list
    where list = [("print", TForall $ AbstractionInfo (Name "T") StarK (TArrow (TVariable (Name "T")) TUnit)),
                  ("car", TForall $ AbstractionInfo (Name "T") StarK (TArrow (TList . TListInfo . Just $ TVariable (Name "T")) (TVariable (Name "T")))),
                  ("cdr", TForall $ AbstractionInfo (Name "T") StarK (TArrow (TList . TListInfo . Just $ TVariable (Name "T")) (TList . TListInfo . Just $ TVariable (Name "T")))),
                  ("map", mapType),
                  ("filter", filterType),
                  ("fold", foldType),
                  ("foldBack", foldType)]

mapType :: Type
mapType =
  TForall $ AbstractionInfo (Name "A") StarK
  (TForall $ AbstractionInfo (Name "B") StarK
   (TArrow (TArrow (TVariable (Name "A")) (TVariable (Name "B"))) (TArrow (TList . TListInfo . Just $ TVariable (Name "A")) (TList . TListInfo . Just $ TVariable (Name "B")))))

filterType :: Type
filterType =
  TForall $ AbstractionInfo (Name "A") StarK
   (TArrow (TArrow (TVariable (Name "A")) TBool) (TArrow (TList . TListInfo . Just $ TVariable (Name "A")) (TList . TListInfo . Just $ TVariable (Name "A"))))

foldType :: Type
foldType =
  TForall $ AbstractionInfo (Name "A") StarK
  (TForall $ AbstractionInfo (Name "B") StarK
   (TArrow (TArrow (TVariable (Name "A")) (TArrow (TVariable (Name "B")) (TVariable (Name "B")))) 
     (TArrow (TVariable (Name "B")) (TArrow (TList . TListInfo . Just $ TVariable (Name "A")) (TVariable (Name "B"))))))

car :: Value -> ResultT Value
car (VList []) = fail "Can't apply 'car' function in empty lists"
car (VList list) = return . head $ list
car _ = fail "Function 'car' can only be applied to lists"

cdr :: Value -> ResultT Value
cdr (VList []) = fail "Can't apply 'cdr' to an empty list"
cdr (VList list) = return . VList . tail $ list
cdr _ = fail "Function 'car' can only be applied to lists"

map' :: Value -> ResultT Value
map' fun =
  let fun' = getFunctionalValue fun
  in return $ VNativeFunction . NativeFunction
      $ \case
         VList list'
           -> VList <$> for list' fun'
         _ -> fail "Expecting a list as an argument for the map function"

filter' :: Value -> ResultT Value
filter' fun =
  let fun' = getFunctionalValue fun
  in return $ VNativeFunction . NativeFunction
      $ \case
         VList list'
           -> VList
                . map fst
                   . filter (\ (_, a) -> a == VLiteral (LBool True)) . zip list'
                <$> for list' fun'
         _ -> fail "Expecting a list as an argument for the filter function"

foldAux :: (Value -> ResultT Value) -> Value -> Value -> ResultT Value
foldAux fun' element acc = do
  next <- fun' acc
  getFunctionalValue next element

getFunctionalValue :: Value -> Value -> ResultT Value
getFunctionalValue (VClosure label body env) = \element -> eval (Map.insert label element env) body
getFunctionalValue (VNativeFunction (NativeFunction fun)) = fun
getFunctionalValue _ = error "Should not happen"
    
fold' :: ([Value] -> [Value]) -> Value -> ResultT Value
fold' transform fun =
   let fun' = getFunctionalValue fun
   in return $ VNativeFunction . NativeFunction
       $ \acc -> return $ VNativeFunction . NativeFunction
          $ \case
             VList list'
               -> foldM (foldAux fun') acc (transform list')
             _ -> fail "Expecting a list as an argument for the fold function"

ourPrint :: Value -> ResultT Value
ourPrint value = do
    liftIO $ print (show value)
    return VUnit
