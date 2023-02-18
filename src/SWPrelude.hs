{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module SWPrelude ( evaluatorPrelude, typerPrelude, aliasPrelude, kindPrelude ) where

import Types
import qualified Data.Map as Map
import Utils ( ResultT, throwError' )
import Control.Monad.IO.Class (liftIO)
import Evaluator
import Data.Text ( Text, unpack, pack )
import Data.Traversable ( for )
import Control.Monad ( foldM )
import Text.Printf ( printf )
import Control.Exception ( IOException, catch, throwIO )
import System.IO.Error ( isDoesNotExistError )

evaluatorPrelude :: Map.Map Text Value
evaluatorPrelude = Map.fromList $
                   map (fmap (VNativeFunction . NativeFunction))
                     [ ("print", ourPrint),
                       ("car", car),
                       ("cdr", cdr),
                       ("map", map'),
                       ("filter", filter'),
                       ("fold", fold' id),
                       ("fold-back", fold' reverse),
                       ("read-lines", readLines),
                       ("is-list-empty", isEmpty),
                       ("string-to-integer", stringToInteger),
                       ("read-file", readFile')] ++ [("T", boolean True), ("F", boolean False)]
  
typerPrelude :: Map.Map Text Type
typerPrelude = Map.fromList list
    where list = [("print", TForall $ AbstractionInfo (Name "T") StarK (TArrow (TVariable (Name "T")) TUnit)),
                  ("car", TForall $ AbstractionInfo (Name "T") StarK (TArrow (TList . TListInfo . Just $ TVariable (Name "T")) (TVariable (Name "T")))),
                  ("cdr", TForall $ AbstractionInfo (Name "T") StarK (TArrow (TList . TListInfo . Just $ TVariable (Name "T")) (TList . TListInfo . Just $ TVariable (Name "T")))),
                  ("map", mapType),
                  ("filter", filterType),
                  ("fold", foldType),
                  ("fold-back", foldType),
                  ("read-lines", readLinesType),
                  ("read-file", readFileType),
                  ("is-list-empty", TForall $ AbstractionInfo (Name "T") StarK (TArrow (TList . TListInfo . Just $ TVariable (Name "T")) TBool)),
                  ("string-to-integer", TArrow TString TInteger),
                  ("T", TBool),
                  ("F", TBool)]

aliasPrelude :: Map.Map Text Type
aliasPrelude = Map.fromList list
  where list = [("String", TString),
                ("Integer", TInteger),
                ("Unit", TUnit),
                ("Bool", TBool),
                ("Rational", TRational),
                ("List", TAbstraction (AbstractionInfo (Name "T") StarK (TList . TListInfo . Just $ TVariable (Name "T"))))]

kindPrelude :: Map.Map TVariableInfo Kind
kindPrelude = Map.fromList [(Name "String", StarK), (Name "Integer", StarK), (Name "Bool", StarK), (Name "Unit", StarK), (Name "Rational", StarK), (Name "List", ArrowK StarK StarK)]

readLinesType :: Type
readLinesType =
  TArrow TString (TList . TListInfo . Just $ TString)

readFileType :: Type
readFileType =
  TArrow TString TString

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

boolean :: Bool -> Value
boolean = VLiteral . LBool

stringToInteger :: Value -> ResultT Value
stringToInteger (VLiteral (LString str)) = pure . VLiteral . LInteger . read $ unpack str
stringToInteger _ = fail "Function 'stringToInteger' can only be used on strings"

isEmpty :: Value -> ResultT Value
isEmpty (VList []) = pure $ boolean False
isEmpty (VList _) = pure $ boolean True
isEmpty _ = fail "Function 'isEmpty' can only be applied to lists"

car :: Value -> ResultT Value
car (VList []) = fail "Can't apply 'car' function in empty lists"
car (VList list) = return . head $ list
car _ = fail "Function 'car' can only be applied to lists"

cdr :: Value -> ResultT Value
cdr (VList []) = fail "Can't apply 'cdr' to an empty list"
cdr (VList list) = return . VList . tail $ list
cdr _ = fail "Function 'car' can only be applied to lists"

safeRead :: String -> IO (Maybe Text)
safeRead path = (fmap (Just . pack) $ readFile path) `catch` handleExists
  where
    handleExists :: IOException -> IO (Maybe Text)
    handleExists e
      | isDoesNotExistError e = return Nothing
      | otherwise = throwIO e

readLines :: Value -> ResultT Value
readLines (VLiteral (LString path)) = do
  maybeContent <- liftIO $ safeRead (unpack path)
  case maybeContent of
    Nothing -> throwError' $ printf "Couldn't find file from path %s" (unpack path)
    Just content -> return . VList $ map (VLiteral . LString . pack) (lines $ unpack content)
readLines _ = fail ""

readFile' :: Value -> ResultT Value
readFile' (VLiteral (LString path)) = do
  maybeContent <- liftIO $ safeRead (unpack path)
  case maybeContent of
    Nothing -> throwError' $ printf "Couldn't find file from path %s" (unpack path)
    Just content -> return . VLiteral $ LString content
readFile' _ = fail ""

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
getFunctionalValue (VClosure label body env) = \element -> evalExpression (Map.insert label element env) body
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
    liftIO $ print value
    return VUnit
