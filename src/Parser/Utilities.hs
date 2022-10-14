module Parser.Utilities where
 
import Text.Parsec

type ParserT st = Parsec [Char] st
  
