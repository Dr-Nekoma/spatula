{-# LANGUAGE OverloadedStrings #-}
module Parser.Types (typeP, typeVariable) where

import Types ( Type(..), TForallInfo(TForallInfo) )
import Parser.Utilities ( ParserT, typeVariableGeneric)
import Text.Parsec
    ( string, spaces, char, anyChar, between, many1, choice, try )
import Data.Text ( pack )

typeP :: ParserT st Type
typeP = choice $ fmap try [typeLiteral, typeArrow, typeForAll, typeVariable]

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

curriedArrow :: [Type] -> Type -> Type
curriedArrow types returnType = Prelude.foldr TArrow returnType types

typeVariable :: ParserT st Type
typeVariable = TVariable <$> typeVariableGeneric

typeArrow :: ParserT st Type
typeArrow =
  let arrow = string "->" *> spaces
      args = between (char '(' *> spaces) (spaces *> char ')') (many1 (spaces *> typeP))
      returnType = spaces *> typeP
  in between (char '(' *> spaces) (spaces *> char ')') (curriedArrow <$> (arrow *> args) <*> returnType)

typeForAll :: ParserT st Type
typeForAll =
  let abstract = between (string "forall" *> spaces) (char '.') (pack <$> many1 anyChar)
      forall = TForallInfo <$> (abstract <* spaces) <*> typeP
  in
  between (char '(' *> spaces) (spaces *> char ')') (TForall <$> forall)
