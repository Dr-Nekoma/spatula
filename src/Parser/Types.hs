{-# LANGUAGE OverloadedStrings #-}
module Parser.Types where

import Types
import Parser.Kinds
import Parser.Utilities ( ParserT, arrowP, skip, variableGeneric, argAnd, Keyword(..))
import Text.Parsec

typeP :: ParserT st Type
typeP = choice $ fmap try [arrowP typeP, typeForAll, typeApplication, typeAnonymousRecord, typeAlias]

typeApplication :: ParserT st Type
typeApplication = foldl TApplication <$> (char '|' *> typeP) <*> many1 (skip *> typeP <* skip) <* char '|'

typeApplied :: ParserT st Type
typeApplied = char '!' *> typeP

typeAlias :: ParserT st Type
typeAlias = TAliasPlaceholder <$> variableGeneric

typeAnonymousRecord :: ParserT st Type
typeAnonymousRecord = TAnonymousRecord <$> (string "{|" *> skip *> many1 (argAnd typeP) <* skip <* string "|}")

typeForAll :: ParserT st Type
typeForAll = do
  _ <- char '(' *> skip *> string (show Forall) <* skip
  typeVariableKind <- AbstractionInfo . Name <$> (variableGeneric <* skip <* char '.' <* skip) <*> kindP
  _ <- skip *> char ';' <* skip
  TForall . typeVariableKind <$> (skip *> typeP <* skip <* char ')')
