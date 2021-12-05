{-# OPTIONS_GHC -Wno-type-defaults #-}

module Main (main) where

import Data.Text.Read (decimal)
import qualified Data.Set as Set (fromList)
import qualified HsAoc2021  as Aoc (day1Part1, day1Part2, day2Part1, day2Part2, Command(..), Direction (..) )
import Relude
import qualified Text.Megaparsec as TM
import qualified Text.Megaparsec.Char as TMC
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Char (isLetter)

main :: (MonadIO m) => m ()
main = do
  runDay1 1 Aoc.day1Part1
  runDay1 2 Aoc.day1Part2

  runDay2 1 Aoc.day2Part1
  runDay2 2 Aoc.day2Part2

  where
    runDay2 :: MonadIO m => Int -> ([Aoc.Command] -> Int) -> m ()
    runDay2 part runDay2Func = do
      input <- readInputOfDay2 "../data/day2/part1/input.txt"
      case input of
          Left _ -> print $ "Failure to parse input of Day 2 / Part " <> show part
          Right cmds -> printResult 2 part $ runDay2Func cmds

    runDay1 :: MonadIO m => Int -> ([Int] -> Int) -> m () 
    runDay1 part f = do
      input <- readInputOfDay1 "../data/day1/part1/input.txt"
      printResult 1 part $ f input

-- Dumb function to print result
printResult :: MonadIO m => Int -> Int -> Int -> m()
printResult day part result =
  print $ "Day " <> show day <> " / Part " <> show part <> ": " <> show result

-- Getting input for day 1
readInputOfDay1 :: MonadIO m => Text -> m [Int]
readInputOfDay1 path = do
  content <- readFileText . toString $ path
  pure $ ints content
  where
    ints = fmap fst . rights . fmap decimal . words

-- Getting input for day 2
readInputOfDay2 :: MonadIO m => Text -> m (Either (TM.ParseErrorBundle Text Void) [Aoc.Command])
readInputOfDay2 path = do
  content <- readFile . toString $ path
  TM.runParserT parseCommands "input" . toText $ content


type Day2InputParserT = TM.ParsecT Void Text

parseCommands :: Day2InputParserT m [Aoc.Command]
parseCommands = do
  TM.sepEndBy parseCommand TMC.space

parseCommand :: Day2InputParserT m Aoc.Command
parseCommand = do
  direction <- parseDirection
  speed <- L.decimal
  return $ Aoc.Command { Aoc.speed = speed , Aoc.direction = direction }

parseDirection :: TM.ParsecT Void Text m Aoc.Direction
parseDirection = do
  rawDirection <- TM.many (TM.satisfy isLetter :: Day2InputParserT m Char)
  _ <- TMC.space
  toDirection . toText $ rawDirection
  where
    toDirection x =
      case x of
        "forward" -> return Aoc.Forward
        "down" -> return Aoc.Down
        "up" -> return Aoc.Up
        _ -> TM.failure Nothing (Set.fromList [])


