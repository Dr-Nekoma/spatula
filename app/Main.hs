module Main (main) where

import Evaluator
import qualified Data.Map as Map

main :: IO ()
main = do
  _ <- return $ eval Map.empty (ELiteral LUnit)
  error "Evaluator is not yet ready!"
