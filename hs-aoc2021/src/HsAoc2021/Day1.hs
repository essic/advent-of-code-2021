{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeApplications #-}

module HsAoc2021.Day1 (runDay1) where

import Data.Text.Read (decimal)
import HsAoc2021.Types
  ( computeAnswerOfTheDay,
    mkPartOne,
    mkPartTwo,
    printAnswerOfTheDay,
    runWrapper,
  )
import Relude
    ( fst,
      snd,
      otherwise,
      ($),
      Monad(return),
      Num((+)),
      Ord((>)),
      Show,
      Foldable(foldl'),
      Int,
      Maybe(Just, Nothing),
      Either(..),
      (.),
      Text,
      MonadIO,
      isLeft,
      rights,
      any,
      (<$>),
      drop,
      reverse,
      readFileText,
      sum,
      words,
      ToString(toString) )
import Safe.Exact (takeExactMay)

type Speed = Int

type Depth = Int

data Direction = Forward | Down | Up
  deriving stock (Show)

data Command = Command
  { direction :: Direction,
    speed :: Speed
  }
  deriving stock (Show)

data Position = Position
  { horizontal :: Int,
    depth :: Int,
    aim :: Int
  }
  deriving stock (Show)

day1Part1 :: [Depth] -> Speed
day1Part1 [] = 0
day1Part1 depths@(x : _) =
  snd $ foldl' computeSpeed (x, 0) depths
  where
    computeSpeed (previousDepth, currentSpeed) currentDepth
      | currentDepth > previousDepth = (currentDepth, currentSpeed + 1)
      | otherwise = (currentDepth, currentSpeed)

day1Part2 :: [Depth] -> Speed
day1Part2 depths =
  execute depths []
  where
    execute xs acc =
      case takeExactMay 3 xs of
        Nothing -> day1Part1 . reverse $ acc
        Just xs' -> execute (drop 1 xs) (sum xs' : acc)

readInputOfDay1 :: (MonadIO m) => Text -> m (Either Text [Int])
readInputOfDay1 path = do
  content <- readFileText . toString $ path
  read content
  where
    parse = (decimal <$>) . words
    read c = do
      let content = parse c
       in return $
            if any isLeft content then Left "An error occured !" else Right . (fst <$>) . rights $ content

runDay1 :: MonadIO m => m ()
runDay1 = do
  p1 <- computeAnswerOfTheDay @Int (mkPartOne 1) readInputOfDay1 $ runWrapper day1Part1
  printAnswerOfTheDay p1

  p2 <- computeAnswerOfTheDay @Int (mkPartTwo 1) readInputOfDay1 $ runWrapper day1Part2
  printAnswerOfTheDay p2
