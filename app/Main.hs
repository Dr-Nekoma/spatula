{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import CL
import Evaluator
import Typer
import Types
import Data.Text ( Text, append, pack)
import qualified Data.Text.IO as TIO
import Text.Parsec (parse, ParseError)
import Parser
import Control.Monad.Except ( runExceptT )
import SWPrelude
import System.IO ( hFlush, stdout )
import System.Console.Haskeline
import Control.Monad.IO.Class
import Control.Monad ( when )
import Data.Foldable ( for_ )

fullExecution :: String -> IO ()
fullExecution content = do
  case parse expressionsP "" content of
    Left errorParse -> printMessage (Left errorParse :: Either ParseError Expression)
    Right asts -> do
      for_ asts
        (\ast -> do typed <- runExceptT $ typeCheck ast
                    case typed of
                      Left errorType -> TIO.putStrLn $ "\ESC[91m" <> errorType
                      Right _ -> do evaluated <- runExceptT $ eval evaluatorPrelude ast
                                    case evaluated of
                                      Left errorEvaluator -> TIO.putStrLn $ "\ESC[91m" <> errorEvaluator
                                      Right result -> printMessage (Right result :: Either Text Value))

repl :: IO ()
repl = do runInputT defaultSettings insertion
  where
      insertion :: InputT IO ()
      insertion = do
        minput <- getInputLine "🥄🔪\ESC[94m|λ>\ESC[00m "
        case minput of
          Nothing -> do
            liftIO $ hFlush stdout
            insertion
          Just command -> do
            liftIO $ hFlush stdout
            if command == ":quit" || command == ":q"
            then return ()
            else do
              liftIO $ fullExecution command
              liftIO $ putStr "\ESC[00m"
              insertion

printMessage :: (Show a, Show b) => Either a b -> IO ()
printMessage (Left error') = TIO.putStrLn $ append (pack "\ESC[91m") (pack $ show error')
printMessage (Right something) = TIO.putStrLn $ append (pack "\ESC[94m") (pack $ show something)

main :: IO ()
main = do
  CommandOptions{..} <- parseArgs
  if justRepl
  then repl
  else
    case file of
      Nothing -> fail "Silverware+ file was not provided"
      Just f -> do
        if invalidInputFile f
        then fail "Silverware+ file does not terminate with .sw"
        else do
          content <- readFile f
          if not (justParse || justTypeCheck || justEvaluate) then fullExecution content
          else do
            let parsed = parse expressionP "" content
            when justParse (either (const $ pure ()) (\x -> printMessage (Right x :: Either ParseError Expression)) parsed)
            case parsed of
              Left errorParse -> printMessage (Left errorParse :: Either ParseError Expression)
              Right ast -> do
                when justTypeCheck (runExceptT (typeCheck ast) >>= printMessage)
                when justEvaluate (putStrLn "\ESC[91m- YOU ARE CRAZY -" >> runExceptT (eval evaluatorPrelude ast) >>= printMessage)
