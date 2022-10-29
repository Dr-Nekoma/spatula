module Parser.Utilities
  (
    ParserT
  , typeVariableGeneric
  , variableGeneric
  , delimiters
  , invalidVariables
  , arrowP
  , listP
  )
where
 
import Text.Parsec ( satisfy, upper, many1, parserFail, Parsec, many, between, char, string, spaces )
import Data.Set ( Set, fromList, member )
import Data.Text ( Text, pack )
import Data.Char ( isAlphaNum, isSymbol )
import Types ( Curryable(..) )

type ParserT st = Parsec [Char] st

typeVariableGeneric :: ParserT st Text
typeVariableGeneric = do
  first <- upper
  rest <- many (satisfy $ or . sequence [isAlphaNum, isSymbol])
  let str = first : rest
  if member str invalidVariables
    then parserFail "Unexpected identifier for type variable name"
    else return (pack str)

delimiters :: [String]
delimiters = ["[", "]", "(", ")", "{", "}"]

keyWords :: [String]
keyWords = ["lambda", "if", "forall"]

invalidVariables :: Set String
invalidVariables = fromList $ keyWords ++ delimiters

variableGeneric :: ParserT st Text
variableGeneric = do
  str <- many1 (satisfy $ or . sequence [isAlphaNum, isSymbol])
  if member str invalidVariables
  then parserFail "Unexpected identifier for variable name"
  else return (pack str)

curriedArrow :: Curryable a => [a] -> a -> a
curriedArrow types returnType = Prelude.foldr kurry returnType types  

listP :: ParserT st a -> ParserT st [a]
listP p = between (char '(' *> spaces) (spaces *> char ')') (many1 (spaces *> p))

arrowP :: Curryable a => ParserT st a -> ParserT st a
arrowP p = 
  let arrow = string "->" *> spaces
      returnType = spaces *> p
  in between (char '(' *> spaces) (spaces *> char ')') (curriedArrow <$> (arrow *> listP p) <*> returnType)
