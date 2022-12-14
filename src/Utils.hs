module Utils ( Result, ResultT, throwError', printMessage ) where

import Control.Monad.Except
import Data.String ( IsString(..) )
import qualified Data.Text.IO as TIO
import Data.Text (append, pack, Text)

type Result a = Either Text a
type ResultT a = ExceptT Text IO a

-- throwError' :: String -> ResultT a
-- throwError' = throwError . pack

throwError' :: (IsString e, MonadError e m) => String -> m a
throwError' = throwError . fromString

printMessage :: (Show a, Show b) => Either a b -> IO ()
printMessage (Left error') = TIO.putStrLn $ append (pack "\ESC[91m") (pack $ show error')
printMessage (Right something) = TIO.putStrLn $ append (pack "\ESC[94m") (pack $ show something)

