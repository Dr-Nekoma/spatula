{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
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
import Data.Text (Text, unpack)
import Control.Monad 
import Control.Monad.Except ( runExceptT )
import Control.Monad.State
import SWPrelude
import qualified Data.Map as Map
import Utils
import Data.Either.Extra
import Data.Foldable
import Data.Maybe
import Data.Bifunctor ( Bifunctor(first, second) )
import Control.Monad.Except
import Options.Applicative.Help (bodyHelp)


type ReplT a = StateT (TyperEnv, EvalEnv) (InputT IO) a

type Command = Either String (SpecialOption, Maybe String)

data SpecialOption =
    Import
  | SType
  | Kind
  | Parsed
  | Quit
  | Env
  deriving (Enum, Bounded)

class Options a where  
    showOptions :: a -> (String, String)
    identifyOption :: String -> a -> (a, Maybe String)

instance Options SpecialOption where
  showOptions Import = (":import ", ":i ")
  showOptions SType = (":type ", ":t ")
  showOptions Kind = (":kind ", ":k ")
  showOptions Quit = (":quit", ":q")
  showOptions Parsed = (":parse ", ":p ")
  showOptions Env = (":env", ":e")
  identifyOption str option = (option, msum $ sequence [stripPrefix long, stripPrefix short] str)
    where (long, short) = showOptions option

findSpecialOption :: (SpecialOption, Maybe String) -> Bool
findSpecialOption = isJust . snd

liftRepl :: IO a -> ReplT a
liftRepl = lift . liftIO

replError :: Text -> ReplT ()
replError = liftRepl . TIO.putStrLn . addErrorColor

replSucess :: Text -> ReplT ()
replSucess = liftRepl . TIO.putStrLn . addSuccessColor

genericReplError :: Show a => a -> ReplT ()
genericReplError = liftRepl . TIO.putStrLn . buildError

genericReplSuccess :: Show a => a -> ReplT ()
genericReplSuccess = liftRepl . TIO.putStrLn . buildMessage

replMessage :: (Show a, Show b) => Either a b -> ReplT ()
replMessage = liftRepl . printMessage

typeCheck :: TyperEnv -> Declaration -> ReplT (Maybe Type)
typeCheck env decl = do
   type' <- liftIO $ runExceptT $ typeCheckDeclaration env decl
   case type' of
     Left errorType -> replError errorType >> pure Nothing
     Right typeEnv ->
       case typeEnv of
         Left t -> pure $ Just t
         Right newTyperEnv -> modify (first (const newTyperEnv)) >> pure Nothing

eval :: EvalEnv -> Declaration -> ReplT (Maybe Value)
eval env decl = do
   value <- liftIO $ runExceptT $ evalDeclaration env decl
   case value of
     Left errorValue -> replError errorValue >> pure Nothing
     Right valueEnv ->
       case valueEnv of
         Left v -> pure $ Just v
         Right newEvalEnv -> modify (second (const newEvalEnv)) >> pure Nothing

typeCheckEval :: Declaration -> ReplT (Maybe (Type, Value))
typeCheckEval decl = do
   (typerEnv, evalEnv) <- get
   type' <- typeCheck typerEnv decl
   case type' of
     Nothing -> Nothing <$ eval evalEnv decl
     Just t -> fmap (t,) <$> eval evalEnv decl

singleExecution :: String -> ReplT ()
singleExecution content = do
  case parse declarationP "" content of
    Left errorParse -> genericReplError (errorParse :: ParseError)
    Right decl -> do
        result <- typeCheckEval decl
        case result of
          Nothing -> pure ()
          Just (_, v) -> genericReplSuccess v

getEnv :: ReplT ()
getEnv = do
   (typerEnv, evalEnv) <- get
   genericReplSuccess typerEnv
   genericReplSuccess ("----------------" :: String)
   genericReplSuccess evalEnv

getKind :: String -> ReplT ()
getKind content = do
  case parse typeP "" content of
    Left errorParse -> error $ show errorParse
    Right type' -> do
      (typerEnv, _) <- get
      kind <- liftRepl $ runExceptT (kindCheckWithEnvironment typerEnv type')
      either replError genericReplSuccess kind

getType :: String -> ReplT ()
getType content = do
  case parse expressionP "" content of
    Left errorParse -> error $ show errorParse
    Right ast -> do
      (typerEnv, _) <- get
      typed <- liftRepl $ runExceptT $ typeCheckExpression typerEnv ast
      either replError genericReplSuccess typed

importFile :: FilePath -> ReplT ()
importFile path = do
  content <- liftRepl $ readFile path
  case parse fileP "" content of
    Left errorParse -> error $ show errorParse
    Right decls -> for_ decls typeCheckEval

getParsed :: String -> ReplT ()
getParsed content = replMessage $ parse declarationP "" content

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
executeSpecialOption Quit = const $ return ()
executeSpecialOption Import = executeCommand importFile
executeSpecialOption Parsed = executeCommand getParsed
executeSpecialOption SType = executeCommand getType 
executeSpecialOption Kind = executeCommand getKind
executeSpecialOption Env = executeCommand (const getEnv)

flushRepl :: ReplT ()
flushRepl = liftRepl $ hFlush stdout

insertion :: ReplT ()
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

runReplT :: IO ((), (TyperEnv, EvalEnv))
runReplT = runInputT defaultSettings $ runStateT insertion (TyperEnv typerPrelude kindPrelude aliasPrelude, evaluatorPrelude)

repl :: IO ()
repl = void runReplT
