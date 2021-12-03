{-# OPTIONS_GHC -Wno-type-defaults #-}
module Main (main) where

import Data.Text.Read (decimal)
import HsAoc2021 (day1Part1, day1Part2)
import Relude


readInputOfDay1 :: MonadIO m => Text -> m [Int]
readInputOfDay1 path = do
  content <- readFileText . toString $ path 
  pure $ ints content
  where
    ints = fmap fst . rights . fmap decimal . words

printResult :: MonadIO m => Int -> Int -> Int -> m()
printResult day part result =
  print $ "Day " <> show day <> " / Part " <> show part <> ": " <> show result

main :: (MonadIO m) => m ()
main = do
  d1p1 <- runDay1Part1
  d1p2 <- runDay1Part2

  printResult 1 1 d1p1
  printResult 1 2 d1p2
    
  where
    inputOfDay1 = readInputOfDay1 "../data/day1/part1/input.txt"
  
    runDay1Part1 =
      day1Part1 <$> inputOfDay1

    runDay1Part2 =
      day1Part2 <$> inputOfDay1

