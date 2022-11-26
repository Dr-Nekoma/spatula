module Main (main) where

import Evaluator
import Typer
import Types()
import Data.Text ( append, pack)
import qualified Data.Text.IO as TIO
import Text.Parsec (parse)
import Parser
import Control.Monad.Trans.Except ( runExceptT )
import SWPrelude
import System.IO ( hFlush, stdout )
import System.Console.Haskeline
import Control.Monad.IO.Class

execute :: String -> IO ()
execute content = do
  case parse expressionP "" content of
    Left errorParse -> TIO.putStrLn $ append (pack "\ESC[91m") (pack $ show errorParse)
    Right ast -> do
      --print ast
      case typeCheck ast of
        Left errorType -> TIO.putStrLn $ append (pack "\ESC[91m") (pack $ show errorType)
        Right _ ->  do evaluated <- runExceptT $ eval evaluatorPrelude ast
                       case evaluated of
                        Left errorEvaluator -> print $ append (pack "\ESC[91m") errorEvaluator
                        Right result -> print result

main :: IO ()
main = do runInputT defaultSettings insertion
  where
      insertion :: InputT IO ()
      insertion = do
        minput <- getInputLine "🥄|λ> "
        case minput of
          Nothing -> do
            liftIO $ hFlush stdout
            insertion
          Just command -> do
            liftIO $ hFlush stdout
            if command == ":quit" || command == ":q"
            then return ()
            else do
              liftIO $ execute command
              liftIO $ putStr "\ESC[00m"
              insertion
