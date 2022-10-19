{-# LANGUAGE OverloadedStrings #-}
module Parser.Expression where

import Types
    ( Expression(EAbstraction, ELiteral, EVariable, ECondition, EApplication, ETypeApplication, ETypeAbstraction), Literal(..))
import Parser.Utilities ( ParserT, variableGeneric, typeVariableGeneric )
import Parser.Literal ( literal )
import Parser.Types ( typeP )
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

exprETypeAbstraction :: ParserT st Expression
exprETypeAbstraction = 
  let typeVariables = many1 (typeVariableGeneric <* spaces)
      curried list expr = foldr ETypeAbstraction expr list
  in between openDelimiter closeDelimiter (curried <$> (string "forall" *> spaces *> typeVariables <* char '.' <* spaces) <*> expressionP)
  
expressionP :: ParserT st Expression
expressionP = choice $ fmap try [exprLiteral, exprVariable, exprCondition, exprApplication, exprETypeAbstraction, exprAbstraction]

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
  let arg_and_type = (,) <$> (char '(' *> spaces *> variableGeneric <* spaces) <*> (typeP <* spaces <* char ')' <* spaces)
  args <- openDelimiter *> many arg_and_type <* closeDelimiter
  (returnType, body) <- (,) <$> (spaces *> char ':' *> optionMaybe typeP <* spaces) <*> expressionP <* closeDelimiter
  let (lastText, lastType) = Prelude.last args
      first = EAbstraction lastText lastType returnType body
  pure $ Prelude.foldr (($ Nothing) . uncurry EAbstraction) first (Prelude.init args)

