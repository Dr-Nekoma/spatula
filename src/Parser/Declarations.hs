{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Parser.Declarations where

import Data.Text ( Text )
import Control.Monad
import Types
import Parser.Kinds
import Parser.Types
import Parser.Expressions
import Parser.Utilities
import Text.Parsec
    ( char, string, optionMaybe, (<|>), many, many1, between, parserFail, choice, try, digit, eof, manyTill, anyChar, manyAccum )

fileP :: ParserT st [Declaration]
fileP = manyTill (skip *> declarationP <* skip) eof

declarationP :: ParserT st Declaration
declarationP = choice $ fmap try [defload, defaliasP, defadtP, defrecordP, defvalP, defunP, defmoduleP, DeclExpr <$> expressionP]

defvalP :: ParserT st Declaration
defvalP = do
  void $ openDelimiter *> skip *> string "define" <* skip
  name <- variableGeneric <* skip
  value <- expressionP <* skip <* closeDelimiter <* skip
  pure $ DeclVal name value

defadtP :: ParserT st Declaration
defadtP = do
  void $ openDelimiter *> skip *> string "defalgebraic" <* skip
  name <- variableGeneric <* skip
  namedKinds <- skip *> optionMaybe (openDelimiter *> many (argAnd kindP) <* closeDelimiter) <* skip
  let nullaryP = (, []) <$> (skip *> variableGeneric <* skip)
      tagP = (,) <$> (skip *> char '(' *> variableGeneric) <*> (skip *> many1 (typeP <* skip)) <* char ')' <* skip
  types <- many1 (skip *> (choice $ fmap try [nullaryP, tagP])) <* skip <* closeDelimiter <* skip
  let algebraic = TAlgebraic types
      t = maybe algebraic (Prelude.foldr fun algebraic) namedKinds
      fun (n, kind) acc = TAbstraction (AbstractionInfo (Name n) kind acc)
  pure $ DeclType name t

defload :: ParserT st Declaration
defload = do
  void $ openDelimiter *> skip *> string "load" <* skip
  DeclLoad <$> (simpleString <* skip <* closeDelimiter)

defaliasP :: ParserT st Declaration
defaliasP = do
  void $ openDelimiter *> skip *> string "defalias" <* skip
  name <- variableGeneric <* skip
  namedKinds <- skip *> optionMaybe (openDelimiter *> many1 (argAnd kindP) <* closeDelimiter) <* skip
  type' <- typeP <* skip <* closeDelimiter <* skip
  let t = maybe type' (Prelude.foldr fun type') namedKinds
      fun (n, kind) acc = TAbstraction (AbstractionInfo (Name n) kind acc)
  pure $ DeclType name t

defrecordP :: ParserT st Declaration
defrecordP = do
  void $ openDelimiter *> skip *> string "defrecord" <* skip
  name <- variableGeneric <* skip
  namedKinds <- skip *> optionMaybe (openDelimiter *> many (argAnd kindP) <* closeDelimiter) <* skip
  let fieldP = (,) <$> (skip *> char '(' *> variableGeneric) <*> (skip *> typeP <* skip <* char ')' <* skip)
  fields <- many1 (skip *> fieldP) <* skip <* closeDelimiter <* skip
  let initial = TNominalRecord name fields
      type' = maybe initial (Prelude.foldr fun initial) namedKinds
      fun (n, kind) acc = TAbstraction (AbstractionInfo (Name n) kind acc)
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
  void $  openDelimiter *> skip *> string "defun" <* skip
  name <- variableGeneric <* skip
  args <- arguments
  (returnType, body) <- (,) <$> (skip *> char ':' *> skip *> typeP <* skip) <*> many1 (expressionP <* skip) <* closeDelimiter <* skip
  pure $ DeclFun name (getFunType args returnType) (foldArgs args (Just returnType) body)

defmoduleP :: ParserT st Declaration
defmoduleP = do
  void $ openDelimiter *> skip *> string "defmodule" <* skip
  name <- variableGeneric <* skip
  decls <- many (skip *> declarationP <* skip) <* closeDelimiter
  pure $ DeclModule name decls
