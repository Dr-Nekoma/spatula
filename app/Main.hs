module Main (main) where

import Evaluator
import Types
import qualified Data.Map as Map

main :: IO ()
main = do
  let result = eval Map.empty (ELiteral LUnit)
  print result
