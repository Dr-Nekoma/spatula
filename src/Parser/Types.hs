{-# LANGUAGE OverloadedStrings #-}
module Parser.Types (typeP) where

import Types ( Type(..), TForallInfo(TForallInfo) )
import Parser.Utilities ( ParserT )
import Parser.Variable ( invalidVariables )
import Text.Parsec
    ( anyChar,
      char,
      satisfy,
      spaces,
      string,
      upper,
      between,
      many1,
      (<|>),
      parserFail )
import Data.Set ( member )
import Data.Char ( isAlphaNum, isSymbol )
import Data.Text ( pack )

typeP :: ParserT st Type
typeP = typeLiteral <|> typeArrow <|> typeForAll <|> typeVariable

typeLiteral :: ParserT st Type
typeLiteral = typeUnit <|> typeInteger <|> typeBool <|> typeRational

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
typeVariable = TVariable <$> variable
  where variable = do
          first <- upper
          rest <- many1 (satisfy $ or . sequence [isAlphaNum, isSymbol])
          let str = first : rest
          if member str invalidVariables
            then parserFail "Unexpected identifier for type variable name"
            else return (pack str)

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
