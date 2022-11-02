{-# LANGUAGE OverloadedStrings #-}
module Parser.Expression (expressionP) where

import Types
    ( Expression(EAbstraction, ELiteral, EVariable, ECondition, EApplication, ETypeAbstraction, ETypeApplication, ELet), LetSort(..), Literal(..))
import Parser.Utilities --( ParserT, variableGeneric, typeVariableGeneric )
import Parser.Literal ( literal )
import Parser.Types
import Parser.Kinds
import Text.Parsec
    ( char, spaces, string, optionMaybe, (<|>), many, many1, between, parserFail, choice, try )

exprLiteral :: ParserT st Expression
exprLiteral = ELiteral <$> literal

exprVariable :: ParserT st Expression
exprVariable = EVariable <$> variableGeneric

exprApplication :: ParserT st Expression
exprApplication = do
  content <- between openDelimiter closeDelimiter (many1 (spaces *> (fmap Left expressionP <|> fmap Right typeP) <* spaces))
  let function acc = either (EApplication acc) (ETypeApplication acc)
  case content of
    [single] -> case single of
                  Left expr -> return $ EApplication expr (ELiteral LUnit)
                  Right _ -> parserFail "Unexpected type for application or type application"
    ((Right _):_) -> parserFail "Unexpected type for application or type application"
    ((Left fun):args) -> return $ foldl function fun args
    _ -> error "This should never happen 💣 | exprApplication and exprETypeApplication"
  
expressionP :: ParserT st Expression
expressionP = choice $ fmap try [exprLiteral, exprVariable, exprCondition, exprApplication, exprAbstraction, letP]

openDelimiter :: ParserT st Char
openDelimiter = char '[' <* spaces

closeDelimiter :: ParserT st Char
closeDelimiter = spaces *> char ']' 

exprCondition :: ParserT st Expression
exprCondition = ECondition <$> (openDelimiter *> string "if" *> expr) <*> expr <*> expr <* closeDelimiter
  where expr = spaces *> expressionP <* spaces

exprAbstraction :: ParserT st Expression
exprAbstraction = do
  openDelimiter *> string "lambda" *> spaces
  let argAnd a = (,) <$> (char '(' *> spaces *> variableGeneric <* spaces) <*> (a <* spaces <* char ')' <* spaces)
  args <- openDelimiter *> many (fmap Left (argAnd typeP) <|> fmap Right (argAnd kindP)) <* closeDelimiter
  (returnType, body) <- (,) <$> (spaces *> char ':' *> optionMaybe typeP <* spaces) <*> expressionP <* closeDelimiter
  let fun (Right item) = ($ Nothing) . uncurry ETypeAbstraction $ item
      fun (Left item) = ($ Nothing) . uncurry EAbstraction $ item
      first = case Prelude.last args of
                Left (lastText, lastType) -> EAbstraction lastText lastType returnType body
                Right (lastText, lastKind) -> ETypeAbstraction lastText lastKind returnType body
  pure $ Prelude.foldr fun first (Prelude.init args)

letP :: ParserT st Expression
letP =  do
  let letSortP = openDelimiter *> ((In <$ string "let-in") <|> (Plus <$ string "let+")) <* spaces
      couple = between openDelimiter closeDelimiter ((,) <$> variableGeneric <*> expressionP) 
      binds = between openDelimiter closeDelimiter (many (spaces *> couple <* spaces))
  ELet <$> letSortP <*> binds <*> expressionP
