{-# LANGUAGE OverloadedStrings #-}
module Parser.Types (typeP, typeVariable) where

import Types
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

typeForAll :: ParserT st Type
typeForAll = undefined
  -- let abstract = between (string "forall" *> spaces) (char '.') (pack <$> many1 anyChar)
  --     forall = TForallInfo <$> (abstract <* spaces) <*> typeP
  -- in
  -- between (char '(' *> spaces) (spaces *> char ')') (TForall <$> forall)

