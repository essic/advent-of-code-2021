{-
Copyright: (c) 2021 essic
SPDX-License-Identifier: NONE
Maintainer: essic <lollancf37@gmail.com>

Advent of code 2021 in Haskell
-}

module HsAoc2021
  ( day1Part1,
    day1Part2,
  )
where

import Relude
import Safe.Exact (takeExactMay)

type Speed = Int
type Depth = Int

day1Part1 :: [Depth] -> Speed
day1Part1 [] = 0
day1Part1 depths@(x : _) =
  snd $ foldl' speed (x, 0) depths
  where
    speed :: (Depth, Speed) -> Depth -> (Depth, Speed)
    speed (previousDepth, currentSpeed) depth
      | depth > previousDepth = (depth, currentSpeed + 1)
      | otherwise = (depth, currentSpeed)

day1Part2 :: [Depth] -> Speed
day1Part2 depths =
  execute depths []
  where
    execute xs acc =
      case takeExactMay 3 xs of
        Nothing -> day1Part1 . reverse $ acc
        Just xs' -> execute (drop 1 xs) (sum xs' : acc)

