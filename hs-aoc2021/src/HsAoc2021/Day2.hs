{-# LANGUAGE DerivingStrategies #-}

module HsAoc2021.Day2 (day2Part1, day2Part2, readInputOfDay2) where

import Data.Char (isLetter)
import qualified Data.Set as Set (fromList)
import HsAoc2021.Types
  ( Command (..),
    Direction (..),
    Position (..),
    AocParserT,
  )
import Relude
  ( Char,
    Either,
    Int,
    Maybe (..),
    MonadIO,
    Num ((*), (+), (-)),
    Text,
    Void,
    fail,
    readFile,
    return,
    toString,
    toText,
    ($),
    (.),
    (<|>),
  )
import qualified Text.Megaparsec as TM
import qualified Text.Megaparsec.Char as TMC
import qualified Text.Megaparsec.Char.Lexer as L

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
readInputOfDay2 :: MonadIO m => Text -> m (Either (TM.ParseErrorBundle Text Void) [Command])
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
