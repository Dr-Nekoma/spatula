{-# LANGUAGE OverloadedStrings #-}
module Parser.Literal where

import Types
import Parser.Types
import Text.Parsec
  
anyBoolean :: ParserT st Literal
anyBoolean = LBool <$> (true <|> false)
   where true = True <$ char 'T'
         false = False <$ char 'F'

anyInteger :: ParserT st Literal
anyInteger = LInteger . read <$> many1 digit

anyUnit :: ParserT st Literal
anyUnit = LUnit <$ string "()"

anyRational :: ParserT st Literal
anyRational = LRational <$> rational
  where rational = (\l _ r -> read $ l ++ "%" ++ r) <$> numbers <*> char '/' <*> numbers
        numbers = many1 digit

anyLiteral :: ParserT st Literal
anyLiteral = anyUnit <|> anyInteger <|> anyRational <|> anyBoolean
