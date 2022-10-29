{-# LANGUAGE OverloadedStrings #-}
module Parser.Kind ( kindP ) where

import Types
import Parser.Utilities ( ParserT, typeVariableGeneric, arrowP)
import Text.Parsec ( string, choice, try )

kindP :: ParserT st Kind
kindP = choice $ fmap try [kindStar, kindArrow]

kindStar :: ParserT st Kind
kindStar = StarK <$ string "*"

kindArrow :: ParserT st Kind
kindArrow = arrowP kindP