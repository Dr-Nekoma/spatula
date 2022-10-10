 module Parser.Types where
 
import Text.Parsec

type ParserT st = Parsec [Char] st
  
