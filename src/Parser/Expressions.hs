{-# LANGUAGE OverloadedStrings #-}
module Parser.Expressions where

import Types
import Parser.Utilities --( ParserT, variableGeneric, typeVariableGeneric, openDelimiter, closeDelimiter )
import Parser.Types
import Parser.Kinds
import Data.Text (pack)
import Text.Parsec
    ( char, spaces, string, optionMaybe, (<|>), many, many1, between, parserFail, choice, try, digit, eof, manyTill, anyChar )
import Data.Maybe ( fromMaybe )

exprLiteral :: ParserT st Expression
exprLiteral = ELiteral <$> literal

exprVariable :: ParserT st Expression
exprVariable = EVariable <$> variableGeneric

exprApplication :: ParserT st Expression
exprApplication = do
  content <- between openDelimiter closeDelimiter (many1 (spaces *> (fmap Left typeP <|> fmap Right expressionP) <* spaces))
  let function acc = either (ETypeApplication acc) (EApplication acc)
  case content of
    [single] -> case single of
                  Right expr -> return $ EApplication expr (ELiteral LUnit)
                  Left _ -> parserFail "Unexpected type for application or type application"
    ((Left _):_) -> parserFail "Unexpected type for application or type application"
    ((Right fun):args) -> return $ foldl function fun args
    _ -> error "This should never happen 💣 | exprApplication and exprETypeApplication"

expressionsP :: ParserT st [Expression]
expressionsP = many (spaces *> expressionP <* spaces) <* eof
  
expressionP :: ParserT st Expression
expressionP = choice $ fmap try [exprLiteral, exprVariable, exprCondition, exprApplication, exprAbstraction, letP, operatorP, literalListP]

exprCondition :: ParserT st Expression
exprCondition = ECondition <$> (openDelimiter *> string "if" *> expr) <*> expr <*> expr <* closeDelimiter
  where expr = spaces *> expressionP <* spaces

exprAbstraction :: ParserT st Expression
exprAbstraction = do
  openDelimiter *> string "lambda" *> spaces
  let argAnd a = (,) <$> (char '(' *> spaces *> variableGeneric <* spaces) <*> (a <* spaces <* char ')' <* spaces)
  args <- openDelimiter *> many (fmap Left (argAnd typeP) <|> fmap (\(a,b) -> Right (Name a, b)) (argAnd kindP)) <* closeDelimiter
  (returnType, body) <- (,) <$> (spaces *> optionMaybe (spaces *> char ':' *> spaces *> typeP <* spaces)) <*> expressionP <* closeDelimiter
  let fun (Right item) = ($ Nothing) . uncurry ETypeAbstraction $ item
      fun (Left item) = ($ Nothing) . uncurry EAbstraction $ item
      first = case Prelude.last args of
                Left (lastText, lastType) -> EAbstraction lastText lastType returnType body
                Right (lastText, lastKind) -> ETypeAbstraction lastText lastKind returnType body
  pure $ Prelude.foldr fun first (Prelude.init args)

letP :: ParserT st Expression
letP = 
  let letSortP = choice $ fmap (try . (openDelimiter *> spaces *>)) [In <$ string "let-in", Plus <$ string "let+"]
      couple = between openDelimiter closeDelimiter ((,) <$> variableGeneric <*> (spaces *> expressionP <* spaces))
      binds = between openDelimiter closeDelimiter (many (spaces *> couple <* spaces))
  in ELet <$> letSortP <*> (spaces *> binds <* spaces) <*> (expressionP <* spaces <* closeDelimiter)
  
operatorP :: ParserT st Expression
operatorP = 
  let operators = [minBound .. maxBound] :: [Operator]
      operatorsP = choice $ fmap try (map (\x -> x <$ string (show x)) operators)
  in between openDelimiter closeDelimiter (EOperation <$> operatorsP <*> (many1 (spaces *> expressionP) <* spaces))

boolean :: ParserT st Literal
boolean = LBool <$> (true <|> false)
   where true = True <$ char 'T'
         false = False <$ char 'F'

integer :: ParserT st Literal
integer = readInteger <$> optionMaybe (string "-") <*> many1 digit

readInteger :: Maybe String -> [Char] -> Literal
readInteger Nothing = LInteger . read
readInteger (Just _) = LInteger . negate . read

unit :: ParserT st Literal
unit = LUnit <$ string "()"

stringP :: ParserT st Literal
stringP = do
  char '"'
  str <- manyTill anyChar (try $ char '"')
  return $ LString (pack str)

rational :: ParserT st Literal
rational = do
  numerator <- many1 digit <* char '/'
  denominator <- many1 digit
  return $ LRational . read $ numerator ++ "%" ++ denominator 

literalListP :: ParserT st Expression
literalListP =
  let elements = many (spaces *> expressionP <* spaces)
  in between (string "'[") closeDelimiter (EList . fromMaybe [] <$> (optionMaybe elements <* spaces))

literal :: ParserT st Literal
literal = choice $ fmap try [unit, rational, integer, boolean, stringP]