module Parser.Variable where

import Parser.Types
import Types
import Data.Set
import Data.Text
import Text.Parsec
import Data.Char

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
