module Utils ( Result, ResultT, throwError', buildMessage, buildError, printMessage ) where

import Control.Monad.Except
import Data.String ( IsString(..) )
import Data.Text ( Text, pack )
import qualified Data.Text.IO as TIO

type Result a = Either Text a
type ResultT a = ExceptT Text IO a

-- throwError' :: String -> ResultT a
-- throwError' = throwError . pack

throwError' :: (IsString e, MonadError e m) => String -> m a
throwError' = throwError . fromString

buildError :: Show a => a -> Text
buildError error' =  pack $ "\ESC[91m" ++ show error'

buildMessage :: Show a => a -> Text
buildMessage something = pack $ "\ESC[94m" ++ show something

printMessage :: (Show a, Show b) => Either a b -> IO ()
printMessage (Left e) = TIO.putStrLn $ buildError e
printMessage (Right a) = TIO.putStrLn $ buildMessage a

