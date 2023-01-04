{-# LANGUAGE OverloadedStrings #-}
module Parser.Declarations where

import Data.Text ( Text )
import Types
import Parser.Types
import Parser.Expressions
import Parser.Utilities
import Text.Parsec
    ( char, string, optionMaybe, (<|>), many, many1, between, parserFail, choice, try, digit, eof, manyTill, anyChar, manyAccum )

fileP :: ParserT st [Declaration]
fileP = manyTill (skip *> declarationP <* skip) eof

declarationP :: ParserT st Declaration
declarationP = choice $ fmap try [defaliasP, defvalP, defunP, DeclExpr <$> expressionP]

defvalP :: ParserT st Declaration
defvalP = do
  openDelimiter *> skip *> string "define" <* skip
  name <- variableGeneric <* skip
  value <- expressionP <* skip <* closeDelimiter <* skip
  pure $ DeclVal name value

defaliasP :: ParserT st Declaration
defaliasP = do
  openDelimiter *> skip *> string "defalias" <* skip
  name <- variableGeneric <* skip
  type' <- typeP <* skip <* closeDelimiter <* skip
  pure $ DeclType name type'

curriedArrow :: Curryable a => [a] -> a -> a
curriedArrow types returnType = Prelude.foldr kurry returnType types 

getFunType :: [Either (TVariableInfo, Kind) (Text, Type)] -> Type -> Type
getFunType args returnType =
  let fun (Left (i, k)) acc = TForall $ AbstractionInfo i k acc 
      fun (Right (_, t)) acc = TArrow t acc
      first = case Prelude.last args of
                Left (lastText, lastKind) -> TForall $ AbstractionInfo lastText lastKind returnType
                Right (_, lastType) -> TArrow lastType returnType
  in Prelude.foldr fun first (Prelude.init args)

defunP :: ParserT st Declaration
defunP = do
  _ <- openDelimiter *> skip *> string "defun" <* skip
  name <- variableGeneric <* skip
  args <- arguments
  (returnType, body) <- (,) <$> (skip *> char ':' *> skip *> typeP <* skip) <*> many1 (expressionP <* skip) <* closeDelimiter <* skip
  pure $ DeclFun name (getFunType args returnType) (foldArgs args (Just returnType) body)
