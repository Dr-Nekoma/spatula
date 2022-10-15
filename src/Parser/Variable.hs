module Parser.Variable
  (
    anyVariable
  , delimiters
  , invalidVariables
  )
where

import Parser.Utilities ( ParserT )
import Data.Set ( Set, fromList, member )
import Data.Text ( Text, pack )
import Text.Parsec ( satisfy, many1, parserFail )
import Data.Char ( isAlphaNum, isSymbol )

delimiters :: [String]
delimiters = ["[", "]", "(", ")", "{", "}"]

keyWords :: [String]
keyWords = ["lambda", "if"]

invalidVariables :: Set String
invalidVariables = fromList $ keyWords ++ delimiters

anyVariable :: ParserT st Text
anyVariable = do
  str <- many1 (satisfy $ or . sequence [isAlphaNum, isSymbol])
  if member str invalidVariables
  then parserFail "Unexpected identifier for variable name"
  else return (pack str)
