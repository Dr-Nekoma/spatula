module Utils ( Result, ResultT, throwError' ) where

import Control.Monad.Except 
import Data.Text ( Text )
import Data.String ( IsString(..) )

type Result a = Either Text a
type ResultT a = ExceptT Text IO a

-- throwError' :: String -> ResultT a
-- throwError' = throwError . pack

throwError' :: (IsString e, MonadError e m) => String -> m a
throwError' = throwError . fromString

