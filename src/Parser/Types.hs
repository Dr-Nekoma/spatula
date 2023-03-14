{-# LANGUAGE OverloadedStrings #-}
module Parser.Types where

import Types
import Parser.Kinds
import Parser.Utilities ( ParserT, arrowP, skip, variableGeneric, argAnd, Keyword(..), liftParser)
import Text.Parsec
import Control.Monad

typeP :: ParserT st Type
typeP = choice $ fmap try [arrowP typeP, typeForAll, typeApplication, typeAnonymousRecord, typeAlias]

typeApplication :: ParserT st Type
typeApplication = do
  type' <- char '|' *> typeP
  types <- many1 (skip *> typeP <* skip) <* char '|'
  let fun acc element =
        let meta = getMetadata acc
        in FullNode meta (TApplication acc element)
  pure $ foldl fun type' types

typeApplied :: ParserT st Type
typeApplied = char '!' *> typeP

typeAlias :: ParserT st Type
typeAlias = liftParser $ TAliasPlaceholder <$> variableGeneric

typeAnonymousRecord :: ParserT st Type
typeAnonymousRecord = liftParser $ TAnonymousRecord <$> (string "{|" *> skip *> many1 (argAnd typeP) <* skip <* string "|}")

typeForAll :: ParserT st Type
typeForAll = do
  void $ char '(' *> skip *> string (show Forall) <* skip
  typeVariableKind <- AbstractionInfo' . fmap Name <$> (variableGeneric <* skip <* char '.' <* skip) <*> kindP
  void $ skip *> char ';' <* skip
  info <- liftParser $ typeVariableKind <$> (skip *> typeP <* skip <* char ')')
  liftParser . pure $ TForall info
