module Main (main) where

import HsAoc2021
  ( day1Part1,
    day1Part2,
    day2Part1,
    day2Part2,
    day3Part1,
    day3Part2,
    day4Part1,
    day4Part2,
    readInputOfDay1,
    readInputOfDay2,
    readInputOfDay3,
    readInputOfDay4,
  )
import Relude
  ( Either (..),
    Int,
    MonadIO,
    Monoid (mconcat),
    Semigroup ((<>)),
    Text,
    putTextLn,
    show,
    ($),
    (.),
  )
import Relude.Debug (traceShowId)

main :: (MonadIO m) => m ()
main = do
  runDay1 1 day1Part1
  runDay1 2 day1Part2

  runDay2 1 day2Part1
  runDay2 2 day2Part2

  runDay3 1 day3Part1
  runDay3 2 day3Part2

  runDay4 1 day4Part1
  runDay4 2 day4Part2

  where
    runDay1 part = computeAnswerOfTheDay (1, part) readInputOfDay1
    runDay2 part = computeAnswerOfTheDay (2, part) readInputOfDay2
    runDay3 part = computeAnswerOfTheDay (3, part) readInputOfDay3
    runDay4 part = computeAnswerOfTheDay (4, part) readInputOfDay4


computeAnswerOfTheDay :: MonadIO m => (Int, Int) -> (Text -> m (Either b a)) -> (a -> Int) -> m ()
computeAnswerOfTheDay (day, part) readInput compute = do
  input <- readInput $ mconcat ["../data/", "day", show day, ".txt"]
  case input of
    Left _ -> putTextLn $ "Failure to parse input of day" <> show day
    Right x -> printResult day part $ compute x

-- Dumb function to print result
printResult :: MonadIO m => Int -> Int -> Int -> m ()
printResult day part result =
  putTextLn . mconcat $ ["Day ", show day, " / Part ", show part, ": ", show result]
