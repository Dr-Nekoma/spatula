module Parser.Types where

import Types
import Parser.Utilities
import Parser.Literal
import Parser.Variable
import Text.Parsec
import Data.Set
import Data.Char
import Data.Monoid
import Data.Text
import Text.Parsec.Token (GenTokenParser(whiteSpace))

typeP :: ParserT st Type
typeP = undefined

typeUnit :: ParserT st Type
typeUnit = TUnit <$ string "Unit"


typeInteger :: ParserT st Type
typeInteger = TInteger <$ string "Integer"


typeBool :: ParserT st Type
typeBool = TBool <$ string "Bool"


typeRational :: ParserT st Type
typeRational = TRational <$ string "Rational"

typeArrow :: ParserT st Type
typeArrow =
  let arrow = string "->" *> spaces
      args = between (char "(" *> spaces) (spaces *> char ")") (many1 (spaces *> typeP))
      returnP = spaces *> typeP in
  between (char "(" *> spaces) (spaces *> char ")") (TArrow <$> (arrow *> args) <*> returnP)

typeForAll =
  let abstract = between (string "forall" *> spaces) (string ".") (many1 anyChar)
      arrow = (char "(" *> spaces) *> string "->" *> spaces
      args = between (char "(" *> spaces) (spaces *> char ")") (many1 (spaces *> (typeP <|> (many1 anyChar))))
      returnArrow = spaces *> (typeP <|> (many1 anyChar)) <* (spaces *> char ")")
      forall = (abstract *> spaces) <*> (arrow *> args) <*> returnArrow
  in
  between (char "(" *> spaces) (spaces *> char ")") (TForall . TForallInfo <$> forall)
