{-# LANGUAGE DerivingStrategies #-}
module HsAoc2021.Day1 (day1Part1, day1Part2, readInputOfDay1) where

import Data.Text.Read (decimal)
import Relude hiding (Down)
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

readInputOfDay1 :: MonadIO m => Text ->  m (Either Text [Int])
readInputOfDay1 path = do
  content <- readFileText . toString $ path
  read content
  where
    parse = (decimal <$>) . words
    read c = do
      let content = parse c
       in return $
            if any isLeft content then Left "An error occured !" else Right . (fst <$>) . rights $ content
