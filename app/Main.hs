module Main (main) where

import Evaluator
import Typer
import Types
import Data.Text ( append, pack )

main :: IO ()
main = do
  let expr = (ELiteral LUnit)
  case typeCheck expr of
    Left e -> print $ append (pack "\ESC[91m") e
    Right _ -> case eval expr of
                Left e -> print $ append (pack "\ESC[91m") e
                Right result -> print result
