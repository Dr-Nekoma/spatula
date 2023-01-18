{-# LANGUAGE OverloadedStrings #-}
module Utils ( Result, ResultT, throwError', buildMessage, buildError, printMessage, addErrorColor, addSuccessColor, printWarning ) where

import Control.Monad.Except
import Data.String ( IsString(..) )
import qualified Data.Text as T ( Text, pack, filter)
import qualified Data.Text.IO as TIO
import Data.Text (Text)

type Result a = Either T.Text a
type ResultT a = ExceptT T.Text IO a

-- throwError' :: String -> ResultT a
-- throwError' = throwError . pack

throwError' :: (IsString e, MonadError e m) => String -> m a
throwError' = throwError . fromString

buildError :: (Show a) => a -> T.Text
buildError error' = T.pack $ "\ESC[91m" <> show error'

buildMessage :: (Show a ) => a -> T.Text
buildMessage something = T.pack $ "\ESC[94m" <> show something

addErrorColor :: Text -> Text
addErrorColor = (<>) "\ESC[91m"

addSuccessColor :: Text -> Text
addSuccessColor = (<>) "\ESC[94m"

printMessage :: (Show a, Show b) => Either a b -> IO ()
printMessage (Left e) = justPrint $ buildError e
printMessage (Right a) = justPrint $ buildMessage a

justPrint :: T.Text -> IO ()
justPrint = TIO.putStrLn . T.filter (/= '"')

printWarning :: MonadIO m => [Char] -> m ()
printWarning msg = liftIO . putStrLn $ "\ESC[93m" ++ msg
