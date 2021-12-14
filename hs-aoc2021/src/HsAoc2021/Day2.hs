{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeApplications #-}

module HsAoc2021.Day2 (runDay2) where

import Data.Char (isLetter)
import qualified Data.Set as Set (fromList)
import HsAoc2021.Types
  ( AocParserError,
    AocParserT,
    computeAnswerOfTheDay,
    mkPartOne,
    mkPartTwo,
    printAnswerOfTheDay,
    runWrapper,
  )
import Relude hiding (Down)
import qualified Text.Megaparsec as TM
import qualified Text.Megaparsec.Char as TMC
import qualified Text.Megaparsec.Char.Lexer as L

data Direction = Forward | Down | Up
  deriving stock (Show)

data Command = Command
  { direction :: Direction,
    speed :: Int
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

-- Getting input for day 2
readInputOfDay2 :: MonadIO m => Text -> m (Either AocParserError [Command])
readInputOfDay2 path = do
  content <- readFile . toString $ path
  TM.runParserT parseCommands "input" . toText $ content

parseCommands :: AocParserT m [Command]
parseCommands = do
  TM.manyTill parseCommand TM.eof
  where
    parseCommand :: AocParserT m Command
    parseCommand = do
      d <- parseDirection
      s <- L.decimal
      _ <- TMC.newline
      return $ Command {speed = s, direction = d}

    parseDirection :: AocParserT m Direction
    parseDirection = do
      rawDirection <-
        TM.manyTill (TM.satisfy isLetter :: AocParserT m Char) TMC.hspace1
          <|> fail "Failure to parse day 2 input !"
      toDirection . toText $ rawDirection
      where
        toDirection x =
          case x of
            "forward" -> return Forward
            "down" -> return Down
            "up" -> return Up
            _ -> TM.failure Nothing (Set.fromList [])

runDay2 :: MonadIO m => m ()
runDay2 = do
  p1 <- computeAnswerOfTheDay @Int (mkPartOne 2) readInputOfDay2 (runWrapper day2Part1)
  printAnswerOfTheDay p1

  p2 <- computeAnswerOfTheDay @Int (mkPartTwo 2) readInputOfDay2 (runWrapper day2Part2)
  printAnswerOfTheDay p2
