{-# LANGUAGE DerivingStrategies #-}
{-
Copyright: (c) 2021 essic
SPDX-License-Identifier: NONE
Maintainer: essic <lollancf37@gmail.com>

Advent of code 2021 in Haskell
-}

module HsAoc2021
  ( day1Part1,
    day1Part2,
    day2Part1,
    day2Part2,
    Direction(..),
    Command(..))
where

import Relude hiding (Down)
import Safe.Exact (takeExactMay)

type Speed = Int
type Depth = Int

day1Part1 :: [Depth] -> Speed
day1Part1 [] = 0
day1Part1 depths@(x : _) =
  snd $ foldl' computeSpeed (x, 0) depths
  where
    computeSpeed :: (Depth, Speed) -> Depth -> (Depth, Speed)
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

data Direction = Forward | Down | Up
  deriving stock Show


data Command = Command
  {
    direction :: Direction
   ,speed :: Speed
  }
  deriving stock Show

data Position = Position 
  {
    horizontal :: Int
   ,depth :: Int
   ,aim :: Int
  }
  deriving stock Show

defaultPosition :: Position
defaultPosition =
  Position { horizontal = 0, aim = 0 , depth = 0 }

day2Part1 :: [Command] -> Int
day2Part1 cmds =
  execute cmds defaultPosition
  where
    execute [] x = horizontal x * depth x
    execute (y:ys) x = 
      execute ys newPosition  
      where
        newPosition =
          case direction y of
            Forward   -> x { horizontal = horizontal x + speed y }
            Down      -> x { depth = depth x + speed y }
            Up        -> x { depth = depth x - speed y }


day2Part2 :: [Command] -> Int
day2Part2 cmds =
  execute cmds defaultPosition
    where 
      execute [] x = horizontal x * depth x
      execute (y:ys) x =
        let newPosition =
              case direction y of
                  Forward   -> x { horizontal = horizontal x + speed y, depth = depth x + (aim x * speed y) }
                  Down      -> x { aim = aim x + speed y }
                  Up        -> x { aim = aim x - speed y }
          in execute ys newPosition
