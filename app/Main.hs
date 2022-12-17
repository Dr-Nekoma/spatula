{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import CL
import Evaluator
import Typer
import Types
import Data.Text ( Text, append, pack, unpack)
import qualified Data.Text.IO as TIO
import Text.Parsec (parse, ParseError)
import Parser
import Control.Monad.Except ( runExceptT )
import SWPrelude
import Control.Monad ( when, msum )
import Data.Foldable ( for_ )
import qualified Data.Map as Map
import Utils
import Repl

fullExecution :: String -> IO ()
fullExecution content = do
  case parse fileP "" content of
    Left errorParse -> putStrLn . unpack $ buildError (errorParse :: ParseError)
    Right decls -> do
        typeEnv' <- runExceptT $ typeCheckDeclarations (TyperEnv typerPrelude Map.empty) decls
        case typeEnv' of
         Left errorType -> TIO.putStrLn $ "\ESC[91m" <> errorType
         Right typeEnv -> do evalEnv' <- runExceptT $ evalDeclarations evaluatorPrelude decls
                             case evalEnv' of
                               Left errorEvaluator -> TIO.putStrLn $ "\ESC[91m" <> errorEvaluator
                               Right evalEnv -> print "It worked!" --printMessage (Right result :: Either Text Value)

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
          fullExecution content
          -- if not (justParse || justTypeCheck || justEvaluate) then fullExecution content
          -- else do
          --   let parsed = parse expressionP "" content
          --   when justParse (either (const $ pure ()) (\x -> printMessage (Right x :: Either ParseError Expression)) parsed)
          --   case parsed of
          --     Left errorParse -> printMessage (Left errorParse :: Either ParseError Expression)
          --     Right ast -> return ()
--                when justTypeCheck (runExceptT (typeCheck ast) >>= printMessage)
--                when justEvaluate (putStrLn "\ESC[91m- YOU ARE CRAZY -" >> runExceptT (eval evaluatorPrelude ast) >>= printMessage)
