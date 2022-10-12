module Main where

import Test.Hspec
import TestTyper

main :: IO ()
main = hspec $ do
  typeCheckingTest
