{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module SWPrelude ( evaluatorPrelude, typerPrelude, aliasPrelude, kindPrelude ) where

import Types
import qualified Data.Map as Map
import Utils ( ResultT, throwError', throwError'' )
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
                   map (fmap (makeEmptyNode . VNativeFunction . NativeFunction))
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
    where list = [("print", printType),
                  ("car", carType),
                  ("cdr", cdrType),
                  ("map", mapType),
                  ("filter", filterType),
                  ("fold", foldType),
                  ("fold-back", foldType),
                  ("read-lines", readLinesType),
                  ("read-file", readFileType),
                  ("is-list-empty", isListEmptyType),
                  ("string-to-integer", makeEmptyNode $ TArrow (makeEmptyNode TString) (makeEmptyNode TInteger)),
                  ("T", makeEmptyNode TBool),
                  ("F", makeEmptyNode TBool)]

aliasPrelude :: Map.Map Text Type
aliasPrelude = Map.fromList list
  where list = [("String", makeEmptyNode TString),
                ("Integer", makeEmptyNode TInteger),
                ("Unit", makeEmptyNode TUnit),
                ("Bool", makeEmptyNode TBool),
                ("Rational", makeEmptyNode TRational),
                ("List", makeEmptyNode $
                           TAbstraction (makeEmptyNode $
                                          AbstractionInfo'
                                          (makeEmptyNode (Name "T"))
                                          (makeEmptyNode StarK)
                                          (makeEmptyNode . TList . makeEmptyNode . TListInfo' . Just $ makeEmptyNode (TVariable (makeEmptyNode (Name "T"))))))]

kindPrelude :: Map.Map TVariableInfo Kind
kindPrelude = Map.fromList [(makeEmptyNode  $ Name "String", makeEmptyNode StarK),
                            (makeEmptyNode $ Name "Integer", makeEmptyNode StarK),
                            (makeEmptyNode $ Name "Bool", makeEmptyNode StarK),
                            (makeEmptyNode $ Name "Unit", makeEmptyNode StarK),
                            (makeEmptyNode $ Name "Rational", makeEmptyNode StarK),
                            (makeEmptyNode $ Name "List", makeEmptyNode $ ArrowK (makeEmptyNode StarK) (makeEmptyNode StarK))]

carType :: Type
carType = makeEmptyNode $
  TForall $ makeEmptyNode $
    AbstractionInfo' (makeEmptyNode (Name "T")) (makeEmptyNode StarK) $
    makeEmptyNode (TArrow (makeEmptyNode $ TList . makeEmptyNode $ TListInfo' . Just . makeEmptyNode $ TVariable
                             (makeEmptyNode $ Name "T"))
                      (makeEmptyNode $ TVariable (makeEmptyNode $ Name "T")))

cdrType :: Type
cdrType = makeEmptyNode $ 
  TForall $ makeEmptyNode $
    AbstractionInfo' (makeEmptyNode (Name "T")) (makeEmptyNode StarK) $
    makeEmptyNode (TArrow (makeEmptyNode $ TList . makeEmptyNode . TListInfo' . Just . makeEmptyNode $ TVariable
                           (makeEmptyNode $ Name "T")) (makeEmptyNode $ TList . makeEmptyNode $ TListInfo' . Just . makeEmptyNode $
                                                        TVariable (makeEmptyNode $ Name "T")))

isListEmptyType :: Type
isListEmptyType = makeEmptyNode $ TForall $
  makeEmptyNode $ AbstractionInfo' (makeEmptyNode $ Name "T") (makeEmptyNode StarK) $
  makeEmptyNode (TArrow (makeEmptyNode $ TList . makeEmptyNode $ TListInfo' . Just . makeEmptyNode $ TVariable
                         (makeEmptyNode $ Name "T")) (makeEmptyNode TBool))

printType :: Type
printType = makeEmptyNode $
  TForall $ makeEmptyNode $
    AbstractionInfo'
    (makeEmptyNode (Name "T"))
      (makeEmptyNode StarK)
      (makeEmptyNode (TArrow
                       (makeEmptyNode (TVariable (makeEmptyNode (Name "T"))))
                       (makeEmptyNode TUnit)))

readLinesType :: Type
readLinesType =
  makeEmptyNode $ TArrow (makeEmptyNode TString) (makeEmptyNode $ TList . makeEmptyNode $ TListInfo' . Just $ makeEmptyNode TString)

readFileType :: Type
readFileType =
  makeEmptyNode $ TArrow (makeEmptyNode TString) (makeEmptyNode TString)

mapType :: Type
mapType =
  makeEmptyNode . TForall . makeEmptyNode $ AbstractionInfo' (makeEmptyNode (Name "A")) (makeEmptyNode StarK) $
  makeEmptyNode (TForall . makeEmptyNode $ AbstractionInfo' (makeEmptyNode (Name "B")) (makeEmptyNode StarK) $
   makeEmptyNode (TArrow (makeEmptyNode (TArrow (makeEmptyNode . TVariable $ makeEmptyNode (Name "A")) (makeEmptyNode $ TVariable (makeEmptyNode (Name "B")))))
                  (makeEmptyNode (TArrow (makeEmptyNode $ TList . makeEmptyNode $ TListInfo' . Just $ makeEmptyNode . TVariable $ makeEmptyNode (Name "A"))
                                  (makeEmptyNode $ TList . makeEmptyNode $ TListInfo' . Just . makeEmptyNode . TVariable $ makeEmptyNode (Name "B"))))))

filterType :: Type
filterType =
  makeEmptyNode . TForall . makeEmptyNode $ AbstractionInfo' (makeEmptyNode (Name "A")) (makeEmptyNode StarK) $
   makeEmptyNode (TArrow (makeEmptyNode $ TArrow (makeEmptyNode . TVariable $ makeEmptyNode (Name "A")) (makeEmptyNode TBool)) $
                  makeEmptyNode (TArrow (makeEmptyNode $ TList . makeEmptyNode $ TListInfo' . Just . makeEmptyNode . TVariable $ makeEmptyNode (Name "A")) $
                                 makeEmptyNode (TList . makeEmptyNode $ TListInfo' . Just . makeEmptyNode . TVariable $ makeEmptyNode (Name "A"))))

foldType :: Type
foldType =
  makeEmptyNode . TForall . makeEmptyNode $ AbstractionInfo' (makeEmptyNode (Name "A")) (makeEmptyNode StarK) $
  makeEmptyNode (TForall . makeEmptyNode $ AbstractionInfo' (makeEmptyNode (Name "B")) (makeEmptyNode StarK) $
   makeEmptyNode (TArrow (makeEmptyNode $ TArrow (makeEmptyNode $ TVariable (makeEmptyNode $ Name "A"))
                          (makeEmptyNode $ TArrow (makeEmptyNode $ TVariable (makeEmptyNode $ Name "B")) (makeEmptyNode $ TVariable (makeEmptyNode $ Name "B")))) $
     makeEmptyNode (TArrow (makeEmptyNode $ TVariable (makeEmptyNode $ Name "B"))
                    (makeEmptyNode $ TArrow (makeEmptyNode $ TList . makeEmptyNode $ TListInfo' . Just $
                                                  makeEmptyNode $ TVariable (makeEmptyNode $ Name "A"))
                                         (makeEmptyNode $ TVariable (makeEmptyNode $ Name "B"))))))

boolean :: Bool -> Value
boolean = makeEmptyNode . VLiteral . LBool

stringToInteger :: Value -> ResultT Value
stringToInteger (FullNode meta (VLiteral (LString str))) = pure . FullNode meta . VLiteral . LInteger . read $ unpack str
stringToInteger _ = fail "Function 'stringToInteger' can only be used on strings"

isEmpty :: Value -> ResultT Value
isEmpty (FullNode _ (VList [])) = pure $ boolean False
isEmpty (FullNode _ (VList _)) = pure $ boolean True
isEmpty _ = fail "Function 'isEmpty' can only be applied to lists"

car :: Value -> ResultT Value
car (FullNode _ (VList [])) = fail "Can't apply 'car' function in empty lists"
car (FullNode _ (VList list)) = return . head $ list
car _ = fail "Function 'car' can only be applied to lists"

cdr :: Value -> ResultT Value
cdr (FullNode _ (VList [])) = fail "Can't apply 'cdr' to an empty list"
cdr (FullNode meta (VList list)) = return . FullNode meta . VList . tail $ list
cdr _ = fail "Function 'car' can only be applied to lists"

safeRead :: String -> IO (Maybe Text)
safeRead path = (fmap (Just . pack) $ readFile path) `catch` handleExists
  where
    handleExists :: IOException -> IO (Maybe Text)
    handleExists e
      | isDoesNotExistError e = return Nothing
      | otherwise = throwIO e

readLines :: Value -> ResultT Value
readLines (FullNode meta (VLiteral (LString path))) = do
  maybeContent <- liftIO $ safeRead (unpack path)
  case maybeContent of
    Nothing -> throwError'' meta $ printf "Couldn't find file from path %s" (unpack path)
    Just content -> return . FullNode meta . VList $ map (FullNode meta . VLiteral . LString . pack) (lines $ unpack content)
readLines _ = fail ""

readFile' :: Value -> ResultT Value
readFile' (FullNode meta (VLiteral (LString path))) = do
  maybeContent <- liftIO $ safeRead (unpack path)
  case maybeContent of
    Nothing -> throwError'' meta $ printf "Couldn't find file from path %s" (unpack path)
    Just content -> return . FullNode meta . VLiteral $ LString content
readFile' _ = fail ""

map' :: Value -> ResultT Value
map' fun =
  let fun' = getFunctionalValue fun
  in return . makeEmptyNode $ VNativeFunction . NativeFunction
      $ \case
         (FullNode meta (VList list'))
           -> FullNode meta . VList <$> for list' fun'
         _ -> fail "Expecting a list as an argument for the map function"

filter' :: Value -> ResultT Value
filter' fun =
  let fun' = getFunctionalValue fun
  in return . makeEmptyNode $ VNativeFunction . NativeFunction
      $ \case
         (FullNode meta (VList list'))
           -> FullNode meta . VList
                . map fst
                   . filter (\ (_, a) -> a == makeEmptyNode (VLiteral $ LBool True)) . zip list'
                <$> for list' fun'
         _ -> fail "Expecting a list as an argument for the filter function"

foldAux :: (Value -> ResultT Value) -> Value -> Value -> ResultT Value
foldAux fun' element acc = do
  next <- fun' acc
  getFunctionalValue next element

getFunctionalValue :: Value -> Value -> ResultT Value
getFunctionalValue (FullNode _ (VClosure label body env)) = \element -> evalExpression (Map.insert (removeMetadata label) element env) body
getFunctionalValue (FullNode _ (VNativeFunction (NativeFunction fun))) = fun
getFunctionalValue _ = error "Should not happen"
    
fold' :: ([Value] -> [Value]) -> Value -> ResultT Value
fold' transform fun =
   let fun' = getFunctionalValue fun
   in return . makeEmptyNode $ VNativeFunction . NativeFunction
       $ \acc -> return . makeEmptyNode $ VNativeFunction . NativeFunction
          $ \case
             (FullNode _ (VList list'))
               -> foldM (foldAux fun') acc (transform list')
             _ -> fail "Expecting a list as an argument for the fold function"

ourPrint :: Value -> ResultT Value
ourPrint value = do
    liftIO . print $ removeMetadata value
    return $ makeEmptyNode VUnit
