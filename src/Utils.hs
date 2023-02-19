{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module Utils ( Result, ResultT, throwError', throwError'', buildMessage, buildError, printMessage, addErrorColor, addSuccessColor, printWarning ) where

import Control.Monad.Except
import Data.String ( IsString(..) )
import qualified Data.Text as T ( Text, pack, filter)
import qualified Data.Text.IO as TIO
import Data.Text (Text)
import Types
import Text.Parsec

type Result a = Either T.Text a
type ResultT a = ExceptT T.Text IO a

-- throwError' :: String -> ResultT a
-- throwError' = throwError . pack

throwError' :: (IsString e, MonadError e m) => FullNode a -> String -> m a
throwError' node msg = 
  let p = getMetadata node
      line = sourceLine p
      col = sourceColumn p
  in throwError . fromString $ buildErrorLocation (sourceName p) line col ++ "\n" ++ msg

throwError'' :: (IsString e, MonadError e m) => Metadata -> String -> m a
throwError'' p msg = 
  let line = sourceLine p
      col = sourceColumn p
  in throwError . fromString $ buildErrorLocation (sourceName p) line col ++ "\n" ++ msg

buildErrorLocation :: FilePath -> Line -> Column -> String
buildErrorLocation file line col = "In " ++ file ++ " | line " ++ show line ++ ", column " ++ show col

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
printWarning msg = liftIO . putStrLn $ "\ESC[93m" ++ msg ++ "\ESC[00m"
