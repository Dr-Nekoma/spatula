{-# LANGUAGE OverloadedStrings #-}
module Parser.Declarations where

import Types
import Parser.Types
import Parser.Expressions
import Parser.Utilities
import Text.Parsec
    ( char, string, optionMaybe, (<|>), many, many1, between, parserFail, choice, try, digit, eof, manyTill, anyChar )

fileP :: ParserT st [Declaration]
fileP = many (skip *> declarationP <* skip)

declarationP :: ParserT st Declaration
declarationP = choice $ fmap try [DeclExpr <$> expressionP, defunP, defvalP, defaliasP]

defvalP :: ParserT st Declaration
defvalP = do
  openDelimiter *> skip *> string "define" <* skip
  name <- variableGeneric <* skip
  value <- expressionP <* skip <* closeDelimiter <* skip
  pure $ DeclVal name value

defaliasP :: ParserT st Declaration
defaliasP = do
  openDelimiter *> skip *> string "defalias" <* skip
  name <- variableGeneric <* skip <* char '=' <* skip
  type' <- typeP <* skip <* closeDelimiter <* skip
  pure $ DeclType name type'

defunP :: ParserT st Declaration
defunP = do
  let couples = (,) <$> (char '(' *> skip *> variableGeneric <* skip) <*> (typeP <* skip <* char ')' <* skip)
  openDelimiter *> skip *> string "defun" <* skip
  name <- variableGeneric <* skip
  args <- openDelimiter *> many1 (skip *> couples) <* closeDelimiter <* skip
  (returnType, body) <- (,) <$> (skip *> char ':' *> skip *> typeP <* skip) <*> many1 (expressionP <* skip) <* closeDelimiter <* skip
  let fun = ($ Nothing) . uncurry EAbstraction
      first = (\(lastText, lastType) -> EAbstraction lastText lastType (Just returnType) (EProgn body)) $ Prelude.last args
      funBody = Prelude.foldr fun first (Prelude.init args)
      (_, types) = unzip args
  pure $ DeclFun name (curriedArrow types returnType) funBody