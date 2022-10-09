{-# LANGUAGE OverloadedStrings #-}
module Parser where

import Types
import Control.Applicative
import Data.Attoparsec.ByteString
import Data.Attoparsec.ByteString.Char8

anyBoolean :: Parser Literal
anyBoolean = LBool <$> (true <|> false)
  where true = (== 'T') <$> char 'T'
        false =  (== 'F') <$> char 'F'

anyInteger :: Parser Literal
anyInteger = LInteger <$> decimal

anyUnit :: Parser Literal
anyUnit = LUnit <$ string "()"

anyRational :: Parser Literal
anyRational = LRational <$> rational

anyLiteral :: Parser Literal
anyLiteral = anyUnit <|> anyInteger <|> anyRational <|> anyBoolean

test :: IO ()
test = case parseOnly anyLiteral "123" of
         Left e -> print e
         Right r -> print r
