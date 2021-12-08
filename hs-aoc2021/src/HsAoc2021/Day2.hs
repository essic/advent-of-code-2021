{-# LANGUAGE DerivingStrategies #-}

module HsAoc2021.Day2 (day2Part1, day2Part2) where

import HsAoc2021.Types
  ( Command (..),
    Direction (..),
    Position (..),
  )
import Relude (Int, Num ((*), (+), (-)))

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
