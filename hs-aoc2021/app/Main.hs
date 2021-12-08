module Main (main) where

import qualified Control.Monad as TM
import Data.Char (isLetter)
import qualified Data.Set as Set (fromList)
import Data.Text.Read (decimal)
import HsAoc2021
    ( day1Part1,
      day1Part2,
      day2Part1,
      day2Part2,
      day3Part1,
      day3Part2 )
import HsAoc2021.Types as Aoc
  ( Command (..),
    DiagnosticReport,
    Direction (..),
    PowerConsumption,
    mkDiagnosticReport,
  )
import Relude
    ( fst,
      ($),
      Eq((==)),
      Monad(return),
      Functor(fmap),
      Applicative(pure),
      Semigroup((<>)),
      Monoid(mconcat),
      Char,
      Int,
      Maybe(Nothing, Just),
      Either(..),
      (.),
      (||),
      Alternative((<|>)),
      rights,
      readFileText,
      readFile,
      putTextLn,
      show,
      words,
      MonadIO,
      Void,
      ToString(toString),
      ToText(toText),
      Text )
import qualified Text.Megaparsec as TM
import qualified Text.Megaparsec.Char as TMC
import qualified Text.Megaparsec.Char.Lexer as L

main :: (MonadIO m) => m ()
main = do
  runDay1 1 day1Part1
  runDay1 2 day1Part2

  runDay2 1 day2Part1
  runDay2 2 day2Part2

  runDay3 1 day3Part1
  runDay3 2 day3Part2
  where
    runDay3 :: MonadIO m => Int -> (Aoc.DiagnosticReport -> Aoc.PowerConsumption) -> m ()
    runDay3 part runDay3Func = do
      input <- readInputOfDay3 "../data/day3.txt"
      case input of
        Left _ -> putTextLn $ "Failure to parse input of Day 3 / Part " <> show part
        Right report -> printResult 3 part $ runDay3Func report

    runDay2 :: MonadIO m => Int -> ([Aoc.Command] -> Int) -> m ()
    runDay2 part runDay2Func = do
      input <- readInputOfDay2 "../data/day2.txt"
      case input of
        Left _ -> putTextLn $ "Failure to parse input of Day 2 / Part " <> show part
        Right cmds -> printResult 2 part $ runDay2Func cmds

    runDay1 :: MonadIO m => Int -> ([Int] -> Int) -> m ()
    runDay1 part f = do
      input <- readInputOfDay1 "../data/day1.txt"
      printResult 1 part $ f input

-- Dumb function to print result
printResult :: MonadIO m => Int -> Int -> Int -> m ()
printResult day part result =
  putTextLn . mconcat $ ["Day ", show day, " / Part ", show part, ": ", show result]

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
  TM.manyTill parseCommand TM.eof
  where
    parseCommand :: Day2InputParserT m Aoc.Command
    parseCommand = do
      d <- parseDirection
      s <- L.decimal
      _ <- TMC.newline
      return $ Aoc.Command {Aoc.speed = s, Aoc.direction = d}

    parseDirection :: Day2InputParserT m Aoc.Direction
    parseDirection = do
      rawDirection <-
        TM.manyTill (TM.satisfy isLetter :: Day2InputParserT m Char) TMC.hspace1
          <|> TM.fail "Failure to parse day 2 input !"
      toDirection . toText $ rawDirection
      where
        toDirection x =
          case x of
            "forward" -> return Aoc.Forward
            "down" -> return Aoc.Down
            "up" -> return Aoc.Up
            _ -> TM.failure Nothing (Set.fromList [])

-- Getting input for day 3
readInputOfDay3 :: MonadIO m => Text -> m (Either (TM.ParseErrorBundle Text Void) Aoc.DiagnosticReport)
readInputOfDay3 path = do
  content <- readFile . toString $ path
  TM.runParserT parseDiagnosticReport "input" . toText $ content

type Day3InputParserT = TM.ParsecT Void Text

parseDiagnosticReport :: Day3InputParserT m Aoc.DiagnosticReport
parseDiagnosticReport = do
  rawLines <- parseLines <|> TM.fail "We cannot parse Day 3 input !"
  case Aoc.mkDiagnosticReport . toBits $ rawLines of
    Just report -> return report
    Nothing -> TM.failure Nothing (Set.fromList [])
  where
    parseLine = TM.manyTill (TM.satisfy (\c -> c == '1' || c == '0') :: Day3InputParserT m Char) TMC.eol
    parseLines = TM.manyTill parseLine TM.eof
    toBits = (fmap . fmap) (== '1')
