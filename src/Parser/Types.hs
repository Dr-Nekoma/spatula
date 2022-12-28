{-# LANGUAGE OverloadedStrings #-}
module Parser.Types (typeP, typeVariable) where

import Types
import Parser.Kinds
import Parser.Utilities ( ParserT, typeVariableGeneric, arrowP, skip, variableGeneric)
import Text.Parsec

typeP :: ParserT st Type
typeP = choice $ fmap try [typeLiteral, arrowP typeP, typeForAll, typeList, typeVariable, typeAlias]

typeLiteral :: ParserT st Type
typeLiteral = choice $ fmap try [typeUnit, typeInteger, typeBool, typeRational, typeString]

typeUnit :: ParserT st Type
typeUnit = TUnit <$ string "Unit"

typeInteger :: ParserT st Type
typeInteger = TInteger <$ string "Integer"

typeBool :: ParserT st Type
typeBool = TBool <$ string "Bool"

typeRational :: ParserT st Type
typeRational = TRational <$ string "Rational"

typeString :: ParserT st Type
typeString = TString <$ string "String"

typeList :: ParserT st Type
typeList = string "List|" *> (TList . TListInfo . Just <$> typeP) <* char '|'

typeVariable :: ParserT st Type
typeVariable = TVariable . Name <$> typeVariableGeneric

typeAlias :: ParserT st Type
typeAlias = TAliasPlaceHolder <$> (string "@" *> variableGeneric)

-- Type-Forall = Enclosed-Type ("forall" WhiteSpace+ Type-Variable WhiteSpace* "." WhiteSpace* Kind WhiteSpace* ";" WhiteSpace* Type)

typeForAll :: ParserT st Type
typeForAll = do
  something <- between (string "forall" *> skip) (skip *> char ';') (AbstractionInfo . Name <$> (typeVariableGeneric <* skip <* char '.' <* skip) <*> kindP)
  between (char '(' *> skip) (skip *> char ')') (TForall . something <$> typeP)

-- \forall list:( * -> *) -> \forall t: * -> \x: list f -> x
-- forall List (-> * *) -> \forall t: * -> \x: list f -> x  

