{-# LANGUAGE OverloadedStrings #-}
module Parser.Types (typeP, typeVariable) where

import Types
import Parser.Kinds
import Parser.Utilities ( ParserT, typeVariableGeneric, arrowP)
import Text.Parsec

typeP :: ParserT st Type
typeP = choice $ fmap try [typeLiteral, arrowP typeP, typeForAll, typeVariable]

typeLiteral :: ParserT st Type
typeLiteral = choice $ fmap try [typeUnit, typeInteger, typeBool, typeRational]

typeUnit :: ParserT st Type
typeUnit = TUnit <$ string "Unit"

typeInteger :: ParserT st Type
typeInteger = TInteger <$ string "Integer"

typeBool :: ParserT st Type
typeBool = TBool <$ string "Bool"

typeRational :: ParserT st Type
typeRational = TRational <$ string "Rational"

typeVariable :: ParserT st Type
typeVariable = TVariable <$> typeVariableGeneric

-- Type-Forall = Enclosed-Type ("forall" WhiteSpace+ Type-Variable WhiteSpace* "." WhiteSpace* Kind WhiteSpace* ";" WhiteSpace* Type)

typeForAll :: ParserT st Type
typeForAll = do
  something <- between (string "forall" *> spaces) (spaces *> char ';') (TForallInfo <$> (typeVariableGeneric <* spaces <* char '.' <* spaces) <*> kindP)
  between (char '(' *> spaces) (spaces *> char ')') (TForall . something <$> typeP)

