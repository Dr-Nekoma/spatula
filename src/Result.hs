{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
module Result (Result (..)) where

import Data.Text ( Text )

data Result a
  = Ok a
  | Error Text
  deriving (Show, Eq, Ord, Functor, Foldable)

instance Semigroup (Result a) where
    Error a <> Error b = Error $ a <> b
    a      <> _ = a

instance Applicative Result where
    pure          = Ok
    Error  e <*> _ = Error e
    Ok f <*> r = fmap f r

instance Monad Result where
    Error  l >>= _ = Error l
    Ok r >>= k = k r
