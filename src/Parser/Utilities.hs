{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
module Parser.Utilities
  (
    ParserT
  , Keyword(..)
  , Delimiter(..)
  , typeVariableGeneric
  , variableGeneric
  , delimiters
  , invalidVariables
  , arrowP
  , listP
  , openDelimiter
  , closeDelimiter
  , curriedArrow
  , skip
  , argAnd
  , simpleString
  , customParse
  , liftParser
  , variable
  )
where
 
import Text.Parsec
import Data.Set ( Set, fromList, member )
import Data.Text ( Text, pack )
import Data.Char ( isAlphaNum, isSymbol, isAscii )
import Types
import Control.Monad

type ParserT st = Parsec [Char] st

liftParser :: ParserT st a -> ParserT st (FullNode a)
liftParser parser = do
  pos <- getPosition
  FullNode pos <$> parser
  
customParse :: ParserT () a -> FilePath -> IO (Either ParseError (FullNode a))
customParse parser file = parse (liftParser parser) file <$> readFile file

typeVariableGeneric :: ParserT st (FullNode Text)
typeVariableGeneric = do
  char '!' >> do (FullNode meta str) <- variable
                 if member str invalidVariables
                 then parserFail "Unexpected identifier for type variable name"
                 else pure $ FullNode meta (pack str)

openDelimiter :: ParserT st (FullNode Char)
openDelimiter = liftParser $ char '[' <* skip

closeDelimiter :: ParserT st (FullNode Char)
closeDelimiter = liftParser $ skip *> char ']' 

data Delimiter =
    LeftBracket
  | RightBracket
  | LeftParens
  | RightParens
  | LeftBraces
  | RightBraces
  | ListDelimiter
  | AnonymousRecordLeftDelimiter
  | AnonymousRecordRightDelimiter  
  | NominalRecordLeftDelimiter
  | NominalRecordRightDelimiter  
  | BeginCommentBlock
  | CloseCommentBlock
  | LineComment
  | ForallDelimiter  
  deriving (Enum, Bounded)
  
instance Show Delimiter where
  show LeftBracket = "["   
  show RightBracket = "]"   
  show LeftParens = "("   
  show RightParens = ")"   
  show LeftBraces = "{"   
  show RightBraces = "}"
  show ListDelimiter = "'"
  show AnonymousRecordLeftDelimiter = "{|"
  show AnonymousRecordRightDelimiter = "|}"  
  show NominalRecordLeftDelimiter = "{"
  show NominalRecordRightDelimiter = "}"  
  show BeginCommentBlock = "{;"
  show CloseCommentBlock = ";}"
  show LineComment = "//"
  show ForallDelimiter = ","

data Keyword =
    Lambda 
  | If
  | Forall
  | Star 
  | LetIn
  | LetPlus
  | Progn
  | RecordGet
  | RecordSet
  | Match
  | Unit
  | DisjunctivePattern
  | Guard
  | NamedPattern1
  | NamedPattern2
  | Wildcard
  deriving (Enum, Bounded)

instance Show Keyword where
  show Lambda = "lambda"
  show If = "if"
  show Forall = "forall"
  show Star = "Star"
  show LetIn = "let-in"
  show LetPlus = "let-plus"
  show Progn = "progn"
  show RecordGet = "getr"
  show RecordSet = "setr"
  show Match = "match"
  show Unit = "nil"
  show DisjunctivePattern = ":or"
  show Guard = ":when"
  show NamedPattern1 = ":with"
  show NamedPattern2 = ":as"
  show Wildcard = "_"

delimiters :: [String]
delimiters = map show ([minBound .. maxBound] :: [Delimiter])

keyWords :: [String]
keyWords = map show ([minBound .. maxBound] :: [Keyword])

operators :: [String]
operators = map show ([minBound .. maxBound] :: [Operator'])

invalidVariables :: Set String
invalidVariables = fromList $ keyWords ++ delimiters ++ operators

isAllowed :: Char -> Bool
isAllowed = and . sequence [canBe, cantBe]
  where canBe = or . sequence [isAlphaNum, isSymbol, isAscii]
        cantBe c = not . any ((== c) . head) $ " " : delimiters ++ ["\n"]

skip :: ParserT st (FullNode ())
skip = liftParser $ void $ spaces *> many single <* spaces
  where single = try (spaces *> commentBlock <* spaces) <|> try (spaces *> commentLine <* spaces)

commentBlock :: ParserT st (FullNode String)
commentBlock = liftParser $ string (show BeginCommentBlock) *> manyTill anyChar (try (string $ show CloseCommentBlock))

commentLine :: ParserT st (FullNode String)
commentLine = liftParser $ string (show LineComment) *> manyTill anyChar (try (char '\n') <|> try (eof >> char ' '))

variable :: ParserT st (FullNode String)
variable = do
  str <- liftParser $ many1 (satisfy isAllowed)
  notFollowedBy (lookAhead (satisfy (not . isAllowed)))
  pure str
  
variableGeneric :: ParserT st (FullNode Text)
variableGeneric = do
  (FullNode meta str) <- variable
  if member str invalidVariables
  then parserFail "Unexpected identifier for variable name"
  else pure $ FullNode meta (pack str)

argAnd :: ParserT st (FullNode a) -> ParserT st (FullNode Text, FullNode a)
argAnd a = (,) <$> (char '(' *> skip *> choice (fmap try [variableGeneric, liftParser $ pack <$> string "_"]) <* skip) <*> (a <* skip <* char ')' <* skip)

curriedArrow :: Curryable a a => [a] -> a -> a
curriedArrow types returnType = Prelude.foldr kurry returnType types  

listArrowP :: ParserT st a -> ParserT st [a]
listArrowP p = between (char '(' *> skip) (skip *> char ')') (many1 (skip *> p))

listP :: ParserT st a -> ParserT st [a]
listP p = between openDelimiter closeDelimiter (many1 (skip *> p))

arrowP :: Curryable a a => ParserT st a -> ParserT st a
arrowP p = 
  let arrow = string "->" *> skip
      returnType = skip *> p
  in between (char '(' *> skip) (skip *> char ')') (curriedArrow <$> (arrow *> listArrowP p) <*> returnType)

simpleString :: ParserT st (FullNode String)
simpleString = do
  void $ char '"'
  liftParser $ manyTill anyChar (try $ char '"')
