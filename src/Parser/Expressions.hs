{-# LANGUAGE OverloadedStrings #-}
module Parser.Expressions where

import Types
import Parser.Utilities --( ParserT, variableGeneric, typeVariableGeneric, openDelimiter, closeDelimiter )
import Parser.Types
import Parser.Kinds
import Data.Text (Text, pack)
import Text.Parsec
    ( char, string, optionMaybe, (<|>), many, many1, between, parserFail, choice, try, digit, eof, manyTill, anyChar )
import Data.Maybe ( fromMaybe )

exprLiteral :: ParserT st Expression
exprLiteral = ELiteral <$> literal

exprVariable :: ParserT st Expression
exprVariable = EVariable <$> variableGeneric

exprApplication :: ParserT st Expression
exprApplication = do
  content <- between openDelimiter closeDelimiter (many1 (skip *> (fmap Left typeApplied <|> fmap Right expressionP) <* skip))
  let function acc = either (ETypeApplication acc) (EApplication acc)
  case content of
    [single] -> case single of
                  Right expr -> return $ EApplication expr (ELiteral LUnit)
                  Left _ -> parserFail "Unexpected type for application or type application"
    ((Left _):_) -> parserFail "Unexpected type for application or type application"
    ((Right fun):args) -> return $ foldl function fun args
    _ -> error "This should never happen 💣 | exprApplication and exprETypeApplication"

expressionsP :: ParserT st [Expression]
expressionsP = many (skip *> expressionP <* skip) <* eof
  
expressionP :: ParserT st Expression
expressionP = choice $ fmap try [exprLiteral, exprVariable, exprCondition,  exprAbstraction, letP, operatorP, literalListP, prognP, anonymousRecordP, recordProjectionP, recordUpdateP, exprApplication, patternMatchingP]

exprCondition :: ParserT st Expression
exprCondition = ECondition <$> (openDelimiter *> string "if" *> expr) <*> expr <*> expr <* closeDelimiter
  where expr = skip *> expressionP <* skip

arguments :: ParserT st [Either (TVariableInfo, Kind) (Text, Type)]
arguments = openDelimiter *> many (choice $ fmap try [fmap (\(a,b) -> Left (Name a, b)) (argAnd kindP), fmap Right (argAnd typeP)]) <* closeDelimiter

foldArgs :: [Either (TVariableInfo, Kind) (Text, Type)] -> Maybe Type -> [Expression] -> Expression
foldArgs args returnType body =
  let fun (Left item) = ($ Nothing) . uncurry ETypeAbstraction $ item
      fun (Right item) = ($ Nothing) . uncurry EAbstraction $ item
      first = case Prelude.last args of
                Left (lastText, lastType) -> ETypeAbstraction lastText lastType returnType (EProgn body)
                Right (lastText, lastKind) -> EAbstraction lastText lastKind returnType (EProgn body)
  in Prelude.foldr fun first (Prelude.init args)

anonymousRecordP :: ParserT st Expression
anonymousRecordP = EAnonymousRecord <$> (string "{|" *> skip *> many1 (argAnd expressionP) <* skip <* string "|}")
  
exprAbstraction :: ParserT st Expression
exprAbstraction = do
  openDelimiter *> string "lambda" *> skip
  args <- arguments
  (returnType, body) <- (,) <$> (skip *> optionMaybe (skip *> char ':' *> skip *> typeP <* skip)) <*> many1 (expressionP <* skip) <* closeDelimiter
  pure $ foldArgs args returnType body

-- [match v
--   [[Left x] [print Integer x]]
--   [[Right x] [print String x]]]

patternP :: ParserT st Pattern
patternP = between openDelimiter closeDelimiter $ choice $ fmap try [patternLiteral, patternAs, patternDisjunctive, patternSumType, patternWildCard, patternVariable]

guard :: ParserT st Expression
guard = skip *> string ":when" *> skip *> expressionP <* skip

patternWildCard :: ParserT st Pattern
patternWildCard = PWildcard <$ string "_"

patternVariable :: ParserT st Pattern
patternVariable = PVariable <$> variableGeneric

patternLiteral :: ParserT st Pattern
patternLiteral = PLiteral <$> literal

patternAs :: ParserT st Pattern
patternAs = PAs <$> (skip *> string ":with" *> skip *> patternP <* skip) <*> (string ":as" *> skip *> variableGeneric <* skip)

patternDisjunctive :: ParserT st Pattern
patternDisjunctive = do
  first <-  skip *> string ":or" *> skip *> patternP <* skip 
  others <- many1 (patternP <* skip)
  pure $ foldl PDisjunctive first others

patternSumType :: ParserT st Pattern
patternSumType = PSumType <$> (skip *> string "!" *> variableGeneric <* skip) <*> many (skip *> patternP <* skip)

-- [match value
--  [[1 :when [< 1 2]] [print stuff]]]

patternMatchingP :: ParserT st Expression
patternMatchingP = do
  openDelimiter *> skip *> string "match" *> skip
  toMatch <- expressionP <* skip
  let guardedPattern = (,) <$> (skip *> patternP <* skip) <*> (optionMaybe guard <* skip)
      branch = between openDelimiter closeDelimiter ((\(p, g) expr -> (p, g, expr)) <$> (skip *> guardedPattern <* skip) <*> expressionP)
  branches <- many1 (skip *> branch <* skip) <* closeDelimiter
  pure $ EPatternMatching toMatch branches

prognP :: ParserT st Expression
prognP = do
  let elements = many (skip *> expressionP <* skip)
  between (openDelimiter *> string "progn" *> skip) closeDelimiter (EProgn <$> (elements <* skip))

recordProjectionP :: ParserT st Expression
recordProjectionP = between (openDelimiter *> string "getr" *> skip) closeDelimiter (ERecordProjection <$> expressionP <*> (skip *> variableGeneric <* skip))

recordUpdateP :: ParserT st Expression
recordUpdateP = between (openDelimiter *> string "setr" *> skip) closeDelimiter (ERecordUpdate <$> expressionP <*> (skip *> listP (argAnd expressionP) <* skip))

letP :: ParserT st Expression
letP = 
  let letSortP = choice $ fmap (try . (openDelimiter *> skip *>)) [In <$ string "let-in", Plus <$ string "let+"]
      couple = between openDelimiter closeDelimiter ((,) <$> variableGeneric <*> (skip *> expressionP <* skip))
      binds = between openDelimiter closeDelimiter (many (skip *> couple <* skip))
  in ELet <$> letSortP <*> (skip *> binds <* skip) <*> (expressionP <* skip <* closeDelimiter)
  
operatorP :: ParserT st Expression
operatorP = 
  let operators = [minBound .. maxBound] :: [Operator]
      operatorsP = choice $ fmap try (map (\x -> x <$ string (show x)) operators)
  in between openDelimiter closeDelimiter (EOperation <$> operatorsP <*> (many1 (skip *> expressionP) <* skip))

integer :: ParserT st Literal
integer = readInteger <$> optionMaybe (string "-") <*> many1 digit

readInteger :: Maybe String -> [Char] -> Literal
readInteger Nothing = LInteger . read
readInteger (Just _) = LInteger . negate . read

unit :: ParserT st Literal
unit = LUnit <$ string "nil"

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
  let elements = many (skip *> expressionP <* skip)
  in between (string "'[") closeDelimiter (EList . fromMaybe [] <$> (optionMaybe elements <* skip))

literal :: ParserT st Literal
literal = choice $ fmap try [unit, rational, integer, stringP]
