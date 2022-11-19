{-# LANGUAGE OverloadedStrings #-}
module Parser.Kinds ( kindP ) where

import Types
import Parser.Utilities ( ParserT, arrowP)
import Text.Parsec ( string, choice, try )

kindP :: ParserT st Kind
kindP = choice $ fmap try [kindStar, kindArrow]

kindStar :: ParserT st Kind
kindStar = StarK <$ string "Star"

kindArrow :: ParserT st Kind
kindArrow = arrowP kindP