{-# LANGUAGE BangPatterns #-}
module Parser.Utilities
  (
    ParserT
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
  )
where
 
import Text.Parsec
import Data.Set ( Set, fromList, member )
import Data.Text ( Text, pack )
import Data.Char ( isAlphaNum, isSymbol, isAscii )
import Types
import Control.Monad

type ParserT st = Parsec [Char] st

typeVariableGeneric :: ParserT st Text
typeVariableGeneric = do
  char '!' >> do str <- variable
                 if member str invalidVariables
                 then parserFail "Unexpected identifier for type variable name"
                 else return (pack str)

openDelimiter :: ParserT st Char
openDelimiter = char '[' <* skip

closeDelimiter :: ParserT st Char
closeDelimiter = skip *> char ']' 

data Delimiter =
    LeftBracket
  | RightBracket
  | LeftParens
  | RightParens
  | LeftBraces
  | RightBraces
  | ListDelimiter
  | RecordDelimiter
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
  show RecordDelimiter = "|"
  show BeginCommentBlock = "{;"
  show CloseCommentBlock = ";}"
  show LineComment = "//"
  show ForallDelimiter = "."

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
  deriving (Enum, Bounded)

instance Show Keyword where
  show Lambda = "lambda"
  show If = "if"
  show Forall = "forall"
  show Star = "Star"
  show LetIn = "let-in"
  show LetPlus = "let+"
  show Progn = "progn"
  show RecordGet = "getr"
  show RecordSet = "setr"

delimiters :: [String]
delimiters = map show ([minBound .. maxBound] :: [Delimiter])

keyWords :: [String]
keyWords = map show ([minBound .. maxBound] :: [Keyword])

operators :: [String]
operators = map show ([minBound .. maxBound] :: [Operator])

invalidVariables :: Set String
invalidVariables = fromList $ keyWords ++ delimiters ++ operators

isAllowed :: Char -> Bool
isAllowed = and . sequence [canBe, cantBe]
  where canBe = or . sequence [isAlphaNum, isSymbol, isAscii]
        cantBe c = not . any ((== c) . head) $ " " : delimiters ++ ["\n"]

skip :: ParserT st ()
skip = void $ spaces *> many single <* spaces
  where single = try (spaces *> commentBlock <* spaces) <|> try (spaces *> commentLine <* spaces)

commentBlock :: ParserT st String
commentBlock = string (show BeginCommentBlock) *> manyTill anyChar (try (string $ show CloseCommentBlock))

commentLine :: ParserT st String
commentLine = string (show LineComment) *> manyTill anyChar (try (char '\n') <|> try (eof >> char ' '))

variable :: ParserT st String
variable = do
  str <- many1 (satisfy isAllowed)
  notFollowedBy (lookAhead (satisfy (not . isAllowed)))
  return str
  
variableGeneric :: ParserT st Text
variableGeneric = do
  str <- variable
  if member str invalidVariables
  then parserFail "Unexpected identifier for variable name"
  else return (pack str)

argAnd :: ParserT st a -> ParserT st (Text, a)
argAnd a = (,) <$> (char '(' *> skip *> variableGeneric <* skip) <*> (a <* skip <* char ')' <* skip)

curriedArrow :: Curryable a => [a] -> a -> a
curriedArrow types returnType = Prelude.foldr kurry returnType types  

listArrowP :: ParserT st a -> ParserT st [a]
listArrowP p = between (char '(' *> skip) (skip *> char ')') (many1 (skip *> p))

listP :: ParserT st a -> ParserT st [a]
listP p = between openDelimiter closeDelimiter (many1 (skip *> p))

arrowP :: Curryable a => ParserT st a -> ParserT st a
arrowP p = 
  let arrow = string "->" *> skip
      returnType = skip *> p
  in between (char '(' *> skip) (skip *> char ')') (curriedArrow <$> (arrow *> listArrowP p) <*> returnType)
