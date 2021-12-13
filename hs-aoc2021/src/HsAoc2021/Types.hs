{-# LANGUAGE DerivingStrategies #-}

module HsAoc2021.Types
  ( AocParserT,
    traceShowId,
    traceShowWith,
    traceShowM,
    traceShow,
    AocParserError,
  computeAnswerOfTheDay)
where

import Relude
import qualified Text.Megaparsec as TM

type AocParserT = TM.ParsecT Void Text

type AocParserError = TM.ParseErrorBundle Text Void

-- Dumb function to print result
printResult :: (MonadIO m, Show a) => Int -> Int -> a -> m ()
printResult day part result =
  putTextLn . mconcat $ ["Day ", show day, " / Part ", show part, ": ", show result]

computeAnswerOfTheDay :: (MonadIO m, Show c) => (Int, Int) -> (Text -> m (Either b a)) -> (a -> c) -> m ()
computeAnswerOfTheDay (day, part) readInput compute = do
  input <- readInput $ mconcat ["../data/", "day", show day, ".txt"]
  case input of
    Left _ -> putTextLn $ "Failure to parse input of day" <> show day
    Right x -> printResult day part $ compute x

