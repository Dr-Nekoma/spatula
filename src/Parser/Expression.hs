{-# LANGUAGE OverloadedStrings #-}
module Parser.Expression (expressionP) where

import Types
    ( Expression(EAbstraction, ELiteral, EVariable, ECondition) )
import Parser.Utilities ( ParserT )
import Parser.Literal ( anyLiteral )
import Parser.Variable ( anyVariable )
import Parser.Types ( typeP )
import Text.Parsec
    ( char, spaces, string, optionMaybe, (<|>), many )

exprLiteral :: ParserT st Expression
exprLiteral = ELiteral <$> anyLiteral

exprVariable :: ParserT st Expression
exprVariable = EVariable <$> anyVariable

-- exprApplication :: ParserT st Expression
-- exprApplication = undefined
  
expressionP :: ParserT st Expression
expressionP = exprVariable <|> exprCondition <|> exprLiteral <|> exprAbstraction 

openDelimiter :: ParserT st Char
openDelimiter = char '[' <* spaces

closeDelimiter :: ParserT st Char
closeDelimiter = spaces *> char ']' 

exprCondition :: ParserT st Expression
exprCondition = ECondition <$> (openDelimiter *> string "if" *> body) <*> body <*> body <* closeDelimiter
  where body = spaces *> expressionP <* spaces

exprAbstraction :: ParserT st Expression
exprAbstraction = do
  openDelimiter *> string "lambda" *> spaces
  let arg_and_type = (,) <$> (char '(' *> spaces *> anyVariable <* spaces) <*> (typeP <* spaces <* char ')' <* spaces)
  args <- openDelimiter *> many arg_and_type <* closeDelimiter
  (returnType, body) <- (,) <$> (spaces *> optionMaybe typeP <* spaces) <*> expressionP <* closeDelimiter
  let (lastText, lastType) = Prelude.last args
      first = EAbstraction lastText lastType returnType body
  pure $ Prelude.foldr (($ Nothing) . uncurry EAbstraction) first (Prelude.init args)

