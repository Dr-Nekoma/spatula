{-# LANGUAGE OverloadedStrings #-}
module Parser.Literal ( literal ) where

import Types ( Literal(..) )
import Parser.Utilities ( ParserT, variableGeneric )
import Text.Parsec ( char, digit, string, many1, (<|>), try, choice, optionMaybe, between )
  
boolean :: ParserT st Literal
boolean = LBool <$> (true <|> false)
   where true = True <$ char 'T'
         false = False <$ char 'F'

integer :: ParserT st Literal
integer = readInteger <$> optionMaybe (string "-") <*> many1 digit

readInteger :: Maybe String -> [Char] -> Literal
readInteger Nothing = LInteger . read
readInteger (Just _) = LInteger . negate . read

unit :: ParserT st Literal
unit = LUnit <$ string "()"

stringP :: ParserT st Literal
stringP = LString <$> between (char '"') (char '"') variableGeneric

rational :: ParserT st Literal
rational = do
  numerator <- many1 digit <* char '/'
  denominator <- many1 digit
  return $ LRational . read $ numerator ++ "%" ++ denominator 

literal :: ParserT st Literal
literal = choice $ fmap try [unit, rational, integer, boolean, stringP]
