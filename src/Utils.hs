{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module Utils ( Result, ResultT, throwError', throwError'', throwErrorMessage, buildMessage, buildError, printMessage, addErrorColor, addSuccessColor, printWarning, printWarning' ) where

import Control.Monad.Except
import Data.String ( IsString(..) )
import qualified Data.Text as T ( Text, pack, filter)
import qualified Data.Text.IO as TIO
import Data.Text (Text)
import Types
import Text.Parsec

type Result a = Either T.Text a
type ResultT a = ExceptT T.Text IO a

throwErrorMessage :: (IsString e, MonadError e m) => String -> m a
throwErrorMessage = throwError . fromString

throwError' :: (IsString e, MonadError e m) => FullNode a -> String -> m a
throwError' node msg = 
  let p = getMetadata node
  in throwError . fromString $ buildLocation p ++ "\n" ++ msg

throwError'' :: (IsString e, MonadError e m) => Metadata -> String -> m a
throwError'' p msg = throwError . fromString $ buildLocation p ++ "\n" ++ msg

buildLocation :: Metadata -> String
buildLocation meta = "In " ++ file ++ " | line " ++ show line ++ ", column " ++ show col
  where file = sourceName meta
        line = sourceLine meta
        col = sourceColumn meta

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

printWarning :: MonadIO m => FullNode a -> [Char] -> m ()
printWarning (FullNode meta _) = printWarning' meta

printWarning' :: MonadIO m => Metadata -> [Char] -> m ()
printWarning' meta msg = liftIO . putStrLn $ "\ESC[93m" ++ buildLocation meta ++ msg ++ "\ESC[00m"
