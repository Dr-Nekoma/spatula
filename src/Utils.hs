module Utils ( Result, ResultT ) where

import Data.Text
import Control.Monad.Trans.Except

type Result a = Either Text a
type ResultT a = ExceptT Text IO a

