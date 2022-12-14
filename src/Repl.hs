{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Repl where

import Evaluator
import Typer
import Types
import System.IO ( hFlush, stdout )
import System.Console.Haskeline
import Control.Monad.IO.Class
import qualified Data.Text.IO as TIO
import Parser
import Text.Parsec (parse, ParseError)
import Data.List ( stripPrefix, find )
import Data.Text (Text)
import Control.Monad.Except ( runExceptT )
import Control.Monad.State
import SWPrelude
import qualified Data.Map as Map
import Utils
import Data.Either.Extra
import Data.Foldable

type ReplT a = StateT EvalEnv (InputT IO) a

type Command = Either String (SpecialOption, Maybe String)

data SpecialOption =
    Import
  | SType
  | Parsed
  | Quit
  deriving (Enum, Bounded)

class Options a where  
    showOptions :: a -> (String, String)
    identifyOption :: String -> a -> (a, Maybe String)

instance Options SpecialOption where
  showOptions Import = (":import ", ":i ")
  showOptions SType = (":type ", ":t ")
  showOptions Quit = (":quit", ":q")
  showOptions Parsed = (":parse ", ":p ")
  identifyOption str option = (option, msum $ sequence [stripPrefix long, stripPrefix short] str)
    where (long, short) = showOptions option

findSpecialOption :: (SpecialOption, Maybe String) -> Bool
findSpecialOption (_, Nothing) = False
findSpecialOption (_, Just _) = True

liftRepl :: IO a -> ReplT a
liftRepl = lift . liftIO

addDeclaration :: Text -> Expression -> ReplT ()
addDeclaration name body =
   do env <- get
      result <- liftRepl . runExceptT $ evalExpression env body
      case result of
        Left e -> liftRepl $ printMessage (Left e :: Either Text Value)
        Right value -> do
          let newEnv = Map.insert name value env
          put newEnv

singleExecution :: String -> ReplT ()
singleExecution content = do
  case parse declarationP "" content of
    Left errorParse -> liftRepl $ printMessage (Left errorParse :: Either ParseError Expression)
    Right decl -> do
        case decl of
          DeclExpr expr ->
            do type' <- liftRepl . runExceptT $ typeCheckExpression (TyperEnv typerPrelude Map.empty) expr
               case type' of
                 Left errorType -> liftRepl $ printMessage (Left errorType :: Either Text Type)
                 Right _ -> do
                   env <- get        
                   result <- liftRepl . runExceptT $ evalExpression env expr
                   liftRepl $ printMessage result
          DeclDef name body -> addDeclaration name body

getType :: String -> ReplT ()
getType content = do
  case parse expressionP "" content of
    Left errorParse -> error $ show errorParse
    Right ast -> do
      typed <- liftRepl $ runExceptT $ typeCheckExpression (TyperEnv typerPrelude Map.empty) ast
      either (\e -> liftRepl $ TIO.putStrLn $ "\ESC[91m" <> e) (\x -> liftRepl $ printMessage (Right x :: Either Text Type)) typed

importFile :: FilePath -> ReplT ()
importFile path = do
  content <- liftRepl $ readFile path
  case parse fileP "" content of
    Left errorParse -> error $ show errorParse
    Right decls ->
        for_ decls
          (\case
            DeclExpr _ -> return ()
            DeclDef name body -> addDeclaration name body)

getParsed :: String -> ReplT ()
getParsed content = do
  case parse declarationP "" content of
    Left errorParse -> error $ show errorParse
    Right ast -> liftRepl $ printMessage (Right ast :: Either Text Declaration)

executeCommand :: (t -> ReplT ()) -> t -> ReplT ()
executeCommand command str = do
  command str
  liftRepl $ putStr "\ESC[00m"
  insertion

identifySpecialOption :: String -> Command
identifySpecialOption str =
  let option = find findSpecialOption $ map (identifyOption str) ([minBound .. maxBound] :: [SpecialOption])
  in maybeToEither str option

executeSpecialOption :: SpecialOption -> String -> ReplT ()
executeSpecialOption Quit _ = return ()
executeSpecialOption Import remaining = executeCommand importFile remaining
executeSpecialOption Parsed remaining = executeCommand getParsed remaining
executeSpecialOption SType remaining = executeCommand getType remaining

flushRepl :: StateT EvalEnv (InputT IO) ()
flushRepl = liftRepl $ hFlush stdout

insertion :: StateT EvalEnv (InputT IO) ()
insertion = do
  minput <- lift $ getInputLine "🥄\ESC[94m|λ>\ESC[00m "
  case minput of
    Nothing -> flushRepl >> insertion
    Just command -> do
      flushRepl
      case identifySpecialOption command of
        Left command' -> executeCommand singleExecution command'
        Right (option, Just remaining) -> executeSpecialOption option remaining
        _ -> error "This should be impossible"

runReplT :: IO ((), EvalEnv)
runReplT =  runInputT defaultSettings $ runStateT insertion evaluatorPrelude

repl :: IO ()
repl = void runReplT
