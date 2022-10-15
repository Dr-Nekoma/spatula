module Parser.Utilities
  (
    ParserT
  )
where
 
import Text.Parsec ( Parsec )

type ParserT st = Parsec [Char] st
  
