module HsAoc2021.Day1 (day1Part1,day1Part2) where
import Safe.Exact (takeExactMay)
import Relude
    ( snd,
      otherwise,
      ($),
      Num((+)),
      Ord((>)),
      Foldable(foldl'),
      Maybe(Just, Nothing),
      (.),
      drop,
      reverse,
      sum )

import HsAoc2021.Types (Depth,Speed)

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

