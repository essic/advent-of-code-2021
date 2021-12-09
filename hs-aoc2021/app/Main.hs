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
      Text, isLeft )
import qualified Text.Megaparsec as TM
import qualified Text.Megaparsec.Char as TMC
import qualified Text.Megaparsec.Char.Lexer as L
import Relude.Foldable (any)

main :: (MonadIO m) => m ()
main = do
  runDay1 1 day1Part1
  runDay1 2 day1Part2

  runDay2 1 day2Part1
  runDay2 2 day2Part2

  runDay3 1 day3Part1
  runDay3 2 day3Part2
  where
    runDay3 part = computeAnswerOfTheDay (3,part) readInputOfDay3
    runDay2 part = computeAnswerOfTheDay (2,part) readInputOfDay2
    runDay1 part = computeAnswerOfTheDay (1,part) readInputOfDay1

computeAnswerOfTheDay :: MonadIO m => (Int,Int) -> (Text -> m (Either b a)) -> (a -> Int) -> m()
computeAnswerOfTheDay (day,part) readInput compute = do
  input <- readInput $ mconcat ["../data/","day",show day,".txt"]
  case input of
      Left _ -> putTextLn $ "Failure to parse input of day" <> show day
      Right x -> printResult day part $ compute x

-- Dumb function to print result
printResult :: MonadIO m => Int -> Int -> Int -> m ()
printResult day part result =
  putTextLn . mconcat $ ["Day ", show day, " / Part ", show part, ": ", show result]

readInputOfDay1 :: MonadIO m => Text  -> m (Either Text [Int])
readInputOfDay1 path = do
  content <- readFileText . toString $ path
  ints content
  where
    input = fmap decimal . words
    ints c = do
      let content = input c
       in return $
         if any isLeft content then Left "An error occured !" else Right . fmap fst . rights $ content


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
