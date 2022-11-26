module Main (main) where

import Evaluator
import Typer
import Types()
import Data.Text ( append, pack, unpack )
import qualified Data.Text.IO as TIO
import Text.Parsec (parse)
import Parser
import Control.Monad.Trans.Except ( runExceptT )

main :: IO ()
main = do
  content <- readFile "examples/Show.sw"
  --content <- readFile "examples/Sum.sw"
  case parse expressionP "" content of
    Left errorParse -> TIO.putStrLn $ append (pack "\ESC[91m") (pack $ show errorParse)
    Right ast -> do
      print ast
      case typeCheck ast of
        Left errorType -> TIO.putStrLn $ append (pack "\ESC[91m") (pack $ show errorType)
        Right _ ->  do evaluated <- runExceptT $ eval ast
                       case evaluated of
                        Left errorEvaluator -> print $ append (pack "\ESC[91m") errorEvaluator
                        Right result -> print result
