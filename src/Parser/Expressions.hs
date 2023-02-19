{-# LANGUAGE OverloadedStrings #-}
module Parser.Expressions where

import Types
import Parser.Utilities --( ParserT, variableGeneric, typeVariableGeneric, openDelimiter, closeDelimiter )
import Parser.Types
import Parser.Kinds
import Data.Text (Text, pack)
import Text.Parsec
    ( char, string, optionMaybe, (<|>), many, many1, between, parserFail, choice, try, digit, eof, manyTill, anyChar, getPosition, sourceName )
import Data.Maybe ( fromMaybe )
import Control.Monad.IO.Class
import Control.Monad (void)

exprLiteral :: ParserT st Expression
exprLiteral = liftParser $  ELiteral <$> literal

exprVariable :: ParserT st Expression
exprVariable = liftParser $ EVariable <$> variableGeneric

exprApplication :: ParserT st Expression
exprApplication = do
  content <- between openDelimiter closeDelimiter (many1 (skip *> (fmap Left typeApplied <|> fmap Right expressionP) <* skip))
  let function acc@(FullNode meta _) = either (FullNode meta . ETypeApplication acc) (FullNode  meta . EApplication acc)
  case content of
    [single] -> case single of
                  Right expr@(FullNode meta _) -> liftParser . pure $ EApplication expr (FullNode meta (ELiteral (FullNode meta LUnit)))
                  Left _ -> parserFail "Unexpected type for application or type application"
    ((Left _):_) -> parserFail "Unexpected type for application or type application"
    ((Right fun):args) -> return $ foldl function fun args
    _ -> error "This should never happen 💣 | exprApplication and exprETypeApplication"

expressionsP :: ParserT st [Expression]
expressionsP = many (skip *> expressionP <* skip) <* eof
  
expressionP :: ParserT st Expression
expressionP = choice $ fmap try [exprLiteral, exprVariable, exprCondition,  exprAbstraction, letP, operatorP, literalListP, prognP, anonymousRecordP, nominalRecordP, recordProjectionP, recordUpdateP, exprApplication, patternMatchingP]

exprCondition :: ParserT st Expression
exprCondition = liftParser $ ECondition <$> (openDelimiter *> string (show If) *> expr) <*> expr <*> expr <* closeDelimiter
  where expr = skip *> expressionP <* skip

arguments :: ParserT st [Either (TVariableInfo, Kind) (FullNode Text, Type)]
arguments = openDelimiter *> many (choice $ fmap try [fmap (\(a,b) -> Left (Name <$> a, b)) (argAnd kindP), unitCase, fmap Right (argAnd typeP)]) <* closeDelimiter
  where unitCase = Right . (\(FullNode meta _) -> (FullNode meta $ pack "_", FullNode meta TUnit)) <$> liftParser (string "(" <* skip <* string ")" <* skip)
  
foldArgs :: [Either (TVariableInfo, Kind) (FullNode Text, Type)] -> Maybe Type -> [Expression] -> Expression'
foldArgs args returnType body =
  let fun (Left (item@(FullNode meta _), kind)) acc = ETypeAbstraction item kind Nothing (FullNode meta acc)
      fun (Right (text, item@(FullNode meta _))) acc = EAbstraction text item Nothing (FullNode meta acc)
      first = case Prelude.last args of
                Left (lastText@(FullNode meta _), lastType) -> ETypeAbstraction lastText lastType returnType (FullNode meta (EProgn body))
                Right (lastText, lastKind@(FullNode meta _)) -> EAbstraction lastText lastKind returnType (FullNode meta (EProgn body))
  in Prelude.foldr fun first (Prelude.init args)

anonymousRecordP :: ParserT st Expression
anonymousRecordP = liftParser $ EAnonymousRecord <$> (string (show AnonymousRecordLeftDelimiter) *> skip *> many1 (argAnd expressionP) <* skip <* string (show AnonymousRecordRightDelimiter))

nominalRecordP :: ParserT st Expression
nominalRecordP = do
  name <- string (show NominalRecordLeftDelimiter) *> skip *> typeP <* skip <* char '|'
  fields <- many1 (argAnd expressionP) <* skip <* string (show AnonymousRecordRightDelimiter)
  liftParser . pure $ ENominalRecord name fields
  
exprAbstraction :: ParserT st Expression
exprAbstraction = do
  (FullNode meta _) <- liftParser $ openDelimiter *> string (show Lambda) <* skip
  args <- arguments
  (returnType, body) <- (,) <$> (skip *> optionMaybe (skip *> char ':' *> skip *> typeP <* skip)) <*> many1 (expressionP <* skip) <* closeDelimiter
  pure . FullNode meta $ foldArgs args returnType body

patternP :: ParserT st Pattern
patternP = between openDelimiter closeDelimiter $ choice $ fmap try [patternLiteral, patternAs, patternDisjunctive, patternSumType, patternWildCard, patternVariable]

guard :: ParserT st Expression
guard = skip *> string (show Guard) *> skip *> expressionP <* skip

patternWildCard :: ParserT st Pattern
patternWildCard = liftParser $ PWildcard <$ string "_"

patternVariable :: ParserT st Pattern
patternVariable = liftParser $ PVariable <$> variableGeneric

patternLiteral :: ParserT st Pattern
patternLiteral = liftParser $ PLiteral <$> literal

patternAs :: ParserT st Pattern
patternAs = liftParser $ PAs <$> (skip *> string (show NamedPattern1) *> skip *> patternP <* skip) <*> (string (show NamedPattern2) *> skip *> variableGeneric <* skip)

patternDisjunctive :: ParserT st Pattern
patternDisjunctive = do
  first <-  skip *> string (show DisjunctivePattern) *> skip *> patternP <* skip 
  others <- many1 (patternP <* skip)
  let fun acc element =
        let meta = getMetadata acc
        in FullNode meta (PDisjunctive acc element)
  pure $ foldl fun first others

patternSumType :: ParserT st Pattern
patternSumType = liftParser $ PSumType <$> (skip *> string "!" *> variableGeneric <* skip) <*> many (skip *> patternP <* skip)

patternMatchingP :: ParserT st Expression
patternMatchingP = do
  void $ openDelimiter *> skip *> string (show Match) *> skip
  toMatch <- expressionP <* skip
  let guardedPattern = (,) <$> (skip *> patternP <* skip) <*> (optionMaybe guard <* skip)
      branch = between openDelimiter closeDelimiter ((\(p, g) expr -> (p, g, expr)) <$> (skip *> guardedPattern <* skip) <*> expressionP)
  branches <- many1 (skip *> branch <* skip) <* closeDelimiter
  liftParser . pure $ EPatternMatching toMatch branches

prognP :: ParserT st Expression
prognP = do
  let elements = many (skip *> expressionP <* skip)
  liftParser $ between (openDelimiter *> string (show Progn) *> skip) closeDelimiter (EProgn <$> (elements <* skip))

recordProjectionP :: ParserT st Expression
recordProjectionP = liftParser $ between (openDelimiter *> string (show RecordGet) *> skip) closeDelimiter (ERecordProjection <$> expressionP <*> (skip *> variableGeneric <* skip))

recordUpdateP :: ParserT st Expression
recordUpdateP = liftParser $ between (openDelimiter *> string (show RecordSet) *> skip) closeDelimiter (ERecordUpdate <$> expressionP <*> (skip *> listP (argAnd expressionP) <* skip))

letP :: ParserT st Expression
letP = 
  let letSortP = liftParser . choice $ fmap (try . (openDelimiter *> skip *>)) [In <$ string (show LetIn), Plus <$ string (show LetPlus)]
      couple = between openDelimiter closeDelimiter ((,) <$> variableGeneric <*> (skip *> expressionP <* skip))
      binds = between openDelimiter closeDelimiter (many (skip *> couple <* skip))
  in liftParser $ ELet <$> letSortP <*> (skip *> binds <* skip) <*> (expressionP <* skip <* closeDelimiter)
  
operatorP :: ParserT st Expression
operatorP = 
  let operators = [minBound .. maxBound] :: [Operator']
      operatorsP = liftParser $ choice $ fmap try (map (\x -> x <$ string (show x)) operators)
  in liftParser $ between openDelimiter closeDelimiter (EOperation <$> operatorsP <*> (many1 (skip *> expressionP) <* skip))

integer :: ParserT st Literal
integer = liftParser $ readInteger <$> optionMaybe (string "-") <*> many1 digit

readInteger :: Maybe String -> [Char] -> Literal'
readInteger Nothing = LInteger . read
readInteger (Just _) = LInteger . negate . read

unit :: ParserT st Literal
unit = liftParser $ LUnit <$ string (show Unit)

stringP :: ParserT st Literal
stringP = fmap (LString . pack) <$> simpleString

rational :: ParserT st Literal
rational = do
  numerator <- many1 digit <* char '/'
  denominator <- many1 digit
  liftParser . pure $ LRational . read $ numerator ++ "%" ++ denominator 

literalListP :: ParserT st Expression
literalListP =
  let elements = many (skip *> expressionP <* skip)
  in liftParser $ between (string "'[") closeDelimiter (EList . fromMaybe [] <$> (optionMaybe elements <* skip))

literal :: ParserT st Literal
literal = choice $ fmap try [unit, rational, integer, stringP]
