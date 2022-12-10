{-# LANGUAGE OverloadedStrings #-}
module Parser.Declarations where

import Types
import Parser.Types
import Parser.Expressions
import Parser.Utilities
import Text.Parsec
    ( char, spaces, string, optionMaybe, (<|>), many, many1, between, parserFail, choice, try, digit, eof, manyTill, anyChar )

declarationsP :: ParserT st [Declaration]
declarationsP = many (spaces *> declarationP <* spaces) <* eof

declarationP :: ParserT st Declaration
declarationP = choice $ fmap try [DeclExpr <$> expressionP, defunP]

defunP :: ParserT st Declaration
defunP = do
  let couples = (,) <$> (char '(' *> spaces *> variableGeneric <* spaces) <*> (typeP <* spaces <* char ')' <* spaces)
  _ <- openDelimiter *> string "defun" <* spaces
  name <- variableGeneric <* spaces
  args <- openDelimiter *> many1 (spaces *> couples) <* closeDelimiter
  (returnType, body) <- (,) <$> (spaces *> optionMaybe (spaces *> char ':' *> spaces *> typeP <* spaces)) <*> expressionP <* closeDelimiter
  let fun = ($ Nothing) . uncurry EAbstraction
      first = (\(lastText, lastType) -> EAbstraction lastText lastType returnType body) $ Prelude.last args
      funBody = Prelude.foldr fun first (Prelude.init args)
  pure $ DeclFun name returnType funBody
