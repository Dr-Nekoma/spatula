{-# LANGUAGE OverloadedStrings #-}
module Parser.Literal ( literal ) where

import Types ( Literal(..) )
import Parser.Utilities ( ParserT )
import Text.Parsec ( char, digit, string, many1, (<|>), try, choice )
  
boolean :: ParserT st Literal
boolean = LBool <$> (true <|> false)
   where true = True <$ char 'T'
         false = False <$ char 'F'

integer :: ParserT st Literal
integer = LInteger . read <$> many1 digit

unit :: ParserT st Literal
unit = LUnit <$ string "()"

rational :: ParserT st Literal
rational = do
  numerator <- many1 digit <* char '/'
  denomintor <- many1 digit
  return $ LRational . read $ numerator ++ "%" ++ denomintor 

literal :: ParserT st Literal
literal = choice $ fmap try [unit, rational, integer, boolean]
