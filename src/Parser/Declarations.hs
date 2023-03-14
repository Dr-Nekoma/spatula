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
declarationP = choice $ fmap try [defloadP, defaliasP, defadtP, defrecordP, defvalP, defunP, defmoduleP, defexpr]

defexpr :: ParserT st Declaration
defexpr = liftParser $ DeclExpr <$> expressionP

defvalP :: ParserT st Declaration
defvalP = do
  void $ openDelimiter *> skip *> string "define" <* skip
  name <- variableGeneric <* skip
  value <- expressionP <* skip <* closeDelimiter <* skip
  liftParser . pure $ DeclVal name value

defadtP :: ParserT st Declaration
defadtP = do
  void $ openDelimiter *> skip *> string "defalgebraic" <* skip
  name <- variableGeneric <* skip
  namedKinds <- skip *> optionMaybe (openDelimiter *> many (argAnd kindP) <* closeDelimiter) <* skip
  let nullaryP = (, []) <$> (skip *> variableGeneric <* skip)
      tagP = (,) <$> (skip *> char '(' *> variableGeneric) <*> (skip *> many1 (typeP <* skip)) <* char ')' <* skip
  types <- many1 (skip *> (choice $ fmap try [nullaryP, tagP])) <* skip <* closeDelimiter <* skip
  let (FullNode meta'' _, _) = head types
      algebraic = FullNode meta'' $ TAlgebraic types
      t = maybe algebraic (Prelude.foldr fun algebraic) namedKinds
      fun (n@(FullNode meta _), kind) acc@(FullNode meta' _) = FullNode meta' $ TAbstraction . FullNode meta $ AbstractionInfo' (Name <$> n) kind acc
  liftParser . pure $ DeclType name t

defloadP :: ParserT st Declaration
defloadP = do
  (FullNode meta _ ) <- liftParser $ openDelimiter *> skip *> string "load" <* skip
  FullNode meta . DeclLoad <$> simpleString <* skip <* closeDelimiter

defaliasP :: ParserT st Declaration
defaliasP = do
  void $ openDelimiter *> skip *> string "defalias" <* skip
  name <- variableGeneric <* skip
  namedKinds <- skip *> optionMaybe (openDelimiter *> many1 (argAnd kindP) <* closeDelimiter) <* skip
  type' <- typeP <* skip <* closeDelimiter <* skip
  let t = maybe type' (Prelude.foldr fun type') namedKinds
      fun (n@(FullNode meta _), kind) acc@(FullNode meta' _) = FullNode meta' $ TAbstraction . FullNode meta $ AbstractionInfo' (Name <$> n) kind acc
  liftParser . pure $ DeclType name t

defrecordP :: ParserT st Declaration
defrecordP = do
  void $ openDelimiter *> skip *> string "defrecord" <* skip
  name <- variableGeneric <* skip
  namedKinds <- skip *> optionMaybe (openDelimiter *> many (argAnd kindP) <* closeDelimiter) <* skip
  let fieldP = (,) <$> (skip *> char '(' *> variableGeneric) <*> (skip *> typeP <* skip <* char ')' <* skip)
  fields <- many1 (skip *> fieldP) <* skip <* closeDelimiter <* skip
  let (FullNode meta'' _, _) = head fields
      initial = FullNode meta'' $ TNominalRecord name fields
      type' = maybe initial (Prelude.foldr fun initial) namedKinds
      fun (n@(FullNode meta _), kind) acc@(FullNode meta' _) = FullNode meta' $ TAbstraction . FullNode meta $ AbstractionInfo' (Name <$> n) kind acc
  liftParser . pure $ DeclType name type'

getFunType :: [Either (TVariableInfo, Kind) (FullNode Text, Type)] -> Type -> Type'
getFunType args returnType =
  let fun (Left (i@(FullNode meta _), k)) acc = TForall $ FullNode meta $ AbstractionInfo' i k (FullNode meta acc)
      fun (Right (_, t@(FullNode meta _))) acc = TArrow t (FullNode meta acc)
      first = case Prelude.last args of
                Left (lastText@(FullNode meta _), lastKind) -> TForall $ FullNode meta $ AbstractionInfo' lastText lastKind returnType
                Right (_, lastType) -> TArrow lastType returnType
  in Prelude.foldr fun first (Prelude.init args)

defunP :: ParserT st Declaration
defunP = do
  (FullNode origin _) <- liftParser $ openDelimiter *> skip *> string "defun" <* skip
  name <- variableGeneric <* skip
  args <- arguments
  (returnType@(FullNode meta' _), body) <- (,) <$> (skip *> char ':' *> skip *> typeP <* skip) <*> many1 (expressionP <* skip) <* closeDelimiter <* skip
  let (FullNode meta _) = head body
  pure . FullNode origin $ DeclFun name (FullNode meta' (getFunType args returnType)) (FullNode meta (foldArgs args (Just returnType) body))

defmoduleP :: ParserT st Declaration
defmoduleP = do
  void $ openDelimiter *> skip *> string "defmodule" <* skip
  name <- variableGeneric <* skip
  decls <- many (skip *> declarationP <* skip) <* closeDelimiter
  liftParser . pure $ DeclModule name decls
