{-# LANGUAGE OverloadedStrings #-}
module Parser.Types where

import Types
import Parser.Kinds
import Parser.Utilities ( ParserT, typeVariableGeneric, arrowP, skip, variableGeneric)
import Text.Parsec

typeP :: ParserT st Type
typeP = choice $ fmap try [arrowP typeP, typeForAll, typeApplication, typeVariable, typeAlias]

-- typeLiteral :: ParserT st Type
-- typeLiteral = choice $ fmap try [typeUnit, typeInteger, typeBool, typeRational, typeString]

-- typeUnit :: ParserT st Type
-- typeUnit = TUnit <$ string "Unit"

-- typeInteger :: ParserT st Type
-- typeInteger = TInteger <$ string "Integer"

-- typeBool :: ParserT st Type
-- typeBool = TBool <$ string "Bool"

-- typeRational :: ParserT st Type
-- typeRational = TRational <$ string "Rational"

-- typeString :: ParserT st Type
-- typeString = TString <$ string "String"

-- typeList :: ParserT st Type
-- typeList = string "List|" *> (TList . TListInfo . Just <$> typeP) <* char '|'

typeApplication :: ParserT st Type
typeApplication = foldl TApplication <$> (char '|' *> typeP) <*> many1 (skip *> typeP <* skip) <* char '|'

typeVariable :: ParserT st Type
typeVariable = TVariable . Name <$> (string "@" *> variableGeneric)

typeApplied :: ParserT st Type
typeApplied = char '!' *> typeP

typeAlias :: ParserT st Type
typeAlias = TAliasPlaceHolder <$> variableGeneric

-- Type-Forall = Enclosed-Type ("forall" WhiteSpace+ Type-Variable WhiteSpace* "." WhiteSpace* Kind WhiteSpace* ";" WhiteSpace* Type)

typeForAll :: ParserT st Type
typeForAll = do
  char '(' *> skip *> string "forall" <* skip
  typeVariableKind <- AbstractionInfo . Name <$> (variableGeneric <* skip <* char '.' <* skip) <*> kindP
  skip *> char ';' <* skip
  TForall . typeVariableKind <$> (skip *> typeP <* skip <* char ')')
