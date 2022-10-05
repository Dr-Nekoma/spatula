module Main (main) where

import Evaluator
import Types
import qualified Data.Map as Map

main :: IO ()
main = do
  result <- return $ eval Map.empty (ELiteral LUnit)
  print result
