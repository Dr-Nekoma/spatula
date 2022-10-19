module Parser.Utilities
  (
    ParserT
  , typeVariableGeneric
  , variableGeneric
  , delimiters
  , invalidVariables
  )
where
 
import Text.Parsec ( satisfy, upper, many1, parserFail, Parsec, many )
import Data.Set ( Set, fromList, member )
import Data.Text ( Text, pack )
import Data.Char ( isAlphaNum, isSymbol )

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
  
