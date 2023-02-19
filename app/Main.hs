{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import CL
import Evaluator
import Typer
import Types
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

-- testSample = EPatternMatching (ELiteral (LInteger 2)) [(PLiteral (LInteger 2), Nothing, ELiteral LUnit), (PLiteral (LInteger 3), Nothing, ELiteral LUnit), (PVariable "x", Nothing, EProgn [(EVariable "x"), ELiteral LUnit])]

fullExecution :: String -> IO ()
fullExecution filename = do
    parserResult <- customParse fileP filename
    case parserResult of
      Left errorParse -> TIO.putStrLn $ buildError (errorParse :: ParseError)
      Right decls -> do
          typeEnv' <- runExceptT $ typeCheckDeclarations typerInitialEnv decls
          case typeEnv' of
           Left errorType -> TIO.putStrLn $ "\ESC[91m" <> errorType
           Right _ -> do evalEnv' <- runExceptT $ evalDeclarations evaluatorPrelude decls
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
          if not (justParse || justTypeCheck || justEvaluate) then fullExecution f
          else do
             let parsedDecls = parse fileP "" content
             when justParse (either (const $ pure ()) (TIO.putStrLn . foldMap ((`append` "\n") . buildMessage)) parsedDecls)
             case parsedDecls of
               Left errorParse -> TIO.putStrLn $ buildError errorParse
               Right asts -> do
                  when
                    justTypeCheck
                    (runExceptT (typeCheckDeclarations typerInitialEnv asts) >>= printMessage . fmap (`differTyperEnv` typerInitialEnv))
                  when
                    justEvaluate
                    (putStrLn "\ESC[91m- YOU ARE CRAZY - \ESC[00m" >> runExceptT (evalDeclarations evaluatorPrelude asts) >>= printMessage . fmap (`Map.difference` evaluatorPrelude))
