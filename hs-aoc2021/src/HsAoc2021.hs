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
    Direction (..),
    Command (..),
    day3Part1,
    mkDiagnosticReport,
    DiagnosticReport,
    PowerConsumption,
    LifeSupportRating,
    day3Part2,
  )
where

import Data.List ((!!))
import qualified Data.Matrix as M (Matrix (..), fromLists, getCol, getRow, toLists)
import qualified Data.Vector as V
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

defaultPosition :: Position
defaultPosition =
  Position {horizontal = 0, aim = 0, depth = 0}

day2Part1 :: [Command] -> Int
day2Part1 cmds =
  execute cmds defaultPosition
  where
    execute [] x = horizontal x * depth x
    execute (y : ys) x =
      let newPosition =
            case direction y of
              Forward -> x {horizontal = horizontal x + speed y}
              Down -> x {depth = depth x + speed y}
              Up -> x {depth = depth x - speed y}
       in execute ys newPosition

day2Part2 :: [Command] -> Int
day2Part2 cmds =
  execute cmds defaultPosition
  where
    execute [] x = horizontal x * depth x
    execute (y : ys) x =
      let newPosition =
            case direction y of
              Forward -> x {horizontal = horizontal x + speed y, depth = depth x + (aim x * speed y)}
              Down -> x {aim = aim x + speed y}
              Up -> x {aim = aim x - speed y}
       in execute ys newPosition

newtype DiagnosticReport = DR (M.Matrix Bool)

type PowerConsumption = Int

type LifeSupportRating = Int

mkDiagnosticReport :: [[Bool]] -> Maybe DiagnosticReport
mkDiagnosticReport xs =
  case nonEmpty xs of
    Nothing -> Nothing
    Just xs' ->
      let rowsLength = length <$> xs'
          l = head rowsLength
       in if all (== l) rowsLength then Just . DR . M.fromLists $ xs else Nothing

day3Part1 :: DiagnosticReport -> PowerConsumption
day3Part1 (DR x) =
  foldl' (*) 1 $ sumBits <$> [allBitsForGamma, allBitsForEpsilon]
  where
    nbColumns = M.ncols x
    allColumns = [1 .. nbColumns]
    allBitsForGamma = (\c -> getMostSignificantBit $ M.getCol c x) <$> allColumns
    allBitsForEpsilon = (\c -> getLeastSignificantBit $ M.getCol c x) <$> allColumns
    binaryWeightedColumns = (\c -> (2 :: Int) ^ (nbColumns - c)) <$> allColumns
    sumBits = sum . fmap (\(weight, value) -> if value then weight else 0) . zip binaryWeightedColumns

getMostSignificantBit :: (Functor t, Foldable t) => t Bool -> Bool
getMostSignificantBit bools =
  numbersOfTrue >= numberOfFalse
  where
    getNumberOfElem x =
      sum $ (\a -> if a == x then 1 else (0 :: Int)) <$> bools
    numberOfFalse = getNumberOfElem False
    numbersOfTrue = getNumberOfElem True

getLeastSignificantBit :: (Functor t, Foldable t) => t Bool -> Bool
getLeastSignificantBit = not . getMostSignificantBit

day3Part2 :: DiagnosticReport -> LifeSupportRating
day3Part2 (DR x) =
  foldl' (*) 1 [oxigenGeneratorRating, co2ScrubberRating]
  where
    oxigenGeneratorRating = sumBits . V.toList . M.getRow 1 $ execute getMostSignificantBit x 1
    co2ScrubberRating = sumBits . V.toList . M.getRow 1 $ execute getLeastSignificantBit x 1
    nbColumns = M.ncols x
    binaryWeightedColumns = (\c -> (2 :: Int) ^ (nbColumns - c)) <$> [1 .. nbColumns]
    sumBits = sum . fmap (\(weight, value) -> if value then weight else 0) . zip binaryWeightedColumns
    filterMatrixByRow f = M.fromLists . filter f . M.toLists
    execute f mx y
      | M.nrows mx == 1 = mx
      | otherwise =
        let sbToFind = f $ M.getCol y mx
            mx' = filterMatrixByRow (\row -> (row !! (y - 1)) == sbToFind) mx
         in execute f mx' (y + 1)
