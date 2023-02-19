{-# LANGUAGE OverloadedStrings #-}
module Parser.Kinds where

import Types
import Parser.Utilities ( ParserT, arrowP, Keyword(..), liftParser)
import Text.Parsec ( string, choice, try )

kindP :: ParserT st Kind
kindP = choice $ fmap try [kindStar, kindArrow]

kindStar :: ParserT st Kind
kindStar = liftParser $ StarK <$ string (show Star)

kindArrow :: ParserT st Kind
kindArrow = arrowP kindP
