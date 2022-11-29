{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module CL (CommandOptions(..), parseArgs, invalidInputFile) where

import Data.List ( isSuffixOf )
import Options.Applicative
    ( columns,
      fullDesc,
      header,
      help,
      info,
      long,
      prefs,
      short,
      showHelpOnError,
      switch,
      customExecParser,
      optional,
      option,
      str,
      helper )
import qualified System.Console.Terminal.Size as TS

data CommandOptions = CommandOptions
  { justParse :: !Bool
  , justTypeCheck :: !Bool
  , justEvaluate :: !Bool
  , justRepl :: !Bool
  , file :: !(Maybe FilePath)
  , debug :: !Bool
  }
  deriving (Show)

parseArgs :: IO CommandOptions
parseArgs = do
  cols <- maybe 80 TS.width <$> TS.size
  customExecParser
    ( prefs $ columns cols <> showHelpOnError )
    ( info
      ( helper <*> do
          justParse <- switch $
            short 'p' <> long "parse" <>
            help "Execute just parsing with the provided file"
          justTypeCheck <- switch $
            short 't' <> long "type-check" <>
            help "Execute just type check with the provided file"
          justEvaluate <- switch $
            short 'v' <> long "eval" <>
            help "Execute just evaluation from the provided file"
          justRepl <- switch $
            short 'r' <> long "repl" <>
            help "Enter Silverware+'s REPL"
          debug <- switch $
            short 'd' <> long "debug" <>
            help "Enable debug mode"
          file <- optional $ option str
            (short 'f' <> long "file" <>
             help "Path for input file. The file must terminate with .sw")
          pure CommandOptions{..}
      )
      ( fullDesc <> header "=== Spatula Interpreter - Silverware+ for the Daily Meal ===" )
    )

invalidInputFile :: FilePath -> Bool
invalidInputFile = not . isSuffixOf ".sw"
