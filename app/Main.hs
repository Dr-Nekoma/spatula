{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import CL
import Evaluator
import Typer
import Data.Text ( append )
import qualified Data.Text.IO as TIO
import Text.Parsec (parse, ParseError)
import Parser
import Control.Monad.Except ( runExceptT )
import SWPrelude
import Control.Monad ( when )
import qualified Data.Map as Map
import Utils
import Repl


fullExecution :: String -> IO ()
fullExecution content = do
  case parse fileP "" content of
    Left errorParse -> TIO.putStrLn $ buildError (errorParse :: ParseError)
    Right decls -> do
        typeEnv' <- runExceptT $ typeCheckDeclarations typerInitialEnv decls id
        case typeEnv' of
         Left errorType -> TIO.putStrLn $ "\ESC[91m" <> errorType
         Right _ -> do evalEnv' <- runExceptT $ evalDeclarations evaluatorPrelude decls id
                       case evalEnv' of
                         Left errorEvaluator -> TIO.putStrLn $ "\ESC[91m" <> errorEvaluator
                         Right _ -> return ()
                               
typerInitialEnv :: TyperEnv
typerInitialEnv = TyperEnv typerPrelude kindPrelude aliasPrelude

differTyperEnv :: TyperEnv -> TyperEnv -> TyperEnv
differTyperEnv (TyperEnv x y z) (TyperEnv a _ _) = TyperEnv (Map.difference x a) y z

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
             let parsedDecls = parse fileP "" content
             when justParse (either (const $ pure ()) (TIO.putStrLn . foldMap ((`append` "\n") . buildMessage)) parsedDecls)
             case parsedDecls of
               Left errorParse -> TIO.putStrLn $ buildError errorParse
               Right asts -> do
                  when
                    justTypeCheck
                    (runExceptT (typeCheckDeclarations typerInitialEnv asts id) >>= printMessage . fmap (`differTyperEnv` typerInitialEnv))
                  when
                    justEvaluate
                    (putStrLn "\ESC[91m- YOU ARE CRAZY - \ESC[00m" >> runExceptT (evalDeclarations evaluatorPrelude asts id) >>= printMessage . fmap (`Map.difference` evaluatorPrelude))
