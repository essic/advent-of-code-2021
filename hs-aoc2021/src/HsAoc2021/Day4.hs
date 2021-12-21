{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}

module HsAoc2021.Day4 (runDay4) where

import qualified Data.Either.Combinators as EC
import Data.Foldable (foldr')
import qualified Data.Matrix as M
import HsAoc2021.Types
  ( AocParserError,
    AocParserT,
    computeAnswerOfTheDay,
    mkPartOne,
    mkPartTwo,
    printAnswerOfTheDay,
  )
import Relude
import Safe (maximumByMay)
import Safe.Foldable (findJust)
import qualified Text.Megaparsec as TM
import qualified Text.Megaparsec.Char as TMC
import qualified Text.Megaparsec.Char.Lexer as L

type BingoNumber = Int

newtype Score = Score Int
  deriving stock (Eq, Ord)
  deriving newtype (Num, Show)

data Cell = Cell {value :: BingoNumber, isSet :: Bool}
  deriving stock (Show, Eq)

newtype Board = Board (M.Matrix Cell)
  deriving stock (Show, Eq)

data TurnStatus = Loosing | MayWin | Won
  deriving stock (Show, Eq, Ord)

data PlayerStatus = PStatus
  { tStatus :: TurnStatus,
    score :: Score
  }
  deriving stock (Eq, Show)

instance Ord PlayerStatus where
  PStatus {tStatus = Won, score = x} `compare` PStatus {tStatus = Won, score = y} = x `compare` y
  PStatus {tStatus = MayWin, score = x} `compare` PStatus {tStatus = MayWin, score = y} = x `compare` y
  PStatus {tStatus = Loosing, score = x} `compare` PStatus {tStatus = Loosing, score = y} = x `compare` y
  PStatus {tStatus = Won, score = _} `compare` PStatus {tStatus = _, score = _} = GT
  PStatus {tStatus = _, score = _} `compare` PStatus {tStatus = Won, score = _} = LT
  PStatus {tStatus = _, score = _} `compare` PStatus {tStatus = MayWin, score = _} = LT
  PStatus {tStatus = _, score = _} `compare` PStatus {tStatus = Loosing, score = _} = LT

data Player = Player
  { gameBoard :: Board,
    status :: PlayerStatus
  }
  deriving stock (Eq)

newtype Turn = Turn Int
  deriving stock (Eq, Ord)
  deriving newtype (Num, Show)

data BingoGame = BingoGame
  { turn :: Turn,
    bingoNumbers :: [BingoNumber],
    players :: [Player]
  }

day4Part1 :: BingoGame -> Either Text Score
day4Part1 = findFirstWinner

day4Part2 :: BingoGame -> Either Text Score
day4Part2 = findLastWinner

findFirstWinner :: BingoGame -> Either Text Score
findFirstWinner game =
  EC.mapLeft (const "We did not find a winner !") $ score . status . snd <$> playUntilWinnerIsFound game

findLastWinner :: BingoGame -> Either Text Score
findLastWinner game = do
  result <- playUntilWinnerIsFound game
  case players . fst $ result of
    [] -> return . score . status . snd $ result
    _ -> findLastWinner . fst $ result

playUntilWinnerIsFound :: BingoGame -> Either Text (BingoGame, Player)
playUntilWinnerIsFound game = do
  allPlayersAfterTheirMove <-
    maybeToRight ("Could not play turn -> " <> show currentTurn) $
      sequenceA $ (<$> maybeAt (i - 1) ns) . flip update <$> ps
  case maximumByMay (\p1 p2 -> status p1 `compare` status p2) $
    filter (\p -> tStatus (status p) == MayWin) allPlayersAfterTheirMove of
    Nothing -> playUntilWinnerIsFound $ game {players = allPlayersAfterTheirMove, turn = currentTurn + 1}
    Just winnerCandidate ->
      let winnerScore = score . status $ winnerCandidate
          pWithNewScores = declareWinner winnerScore <$> allPlayersAfterTheirMove
          winner = findJust (\p -> tStatus (status p) == Won) pWithNewScores
          loosers = filter (/= winner) pWithNewScores
          nextGame = game {players = loosers}
       in Right (nextGame, winner)
  where
    currentTurn@(Turn i) = turn game
    ns = bingoNumbers game
    ps = players game
    declareWinner s p
      | status p == PStatus {score = s, tStatus = MayWin} =
        p {status = PStatus {score = s, tStatus = Won}}
      | otherwise = p {status = PStatus {score = 0, tStatus = Loosing}}

update :: BingoNumber -> Player -> Player
update n player =
  if canPlayerWin
    then Player {gameBoard = newBoard, status = PStatus {score = computeScore n player', tStatus = MayWin}}
    else Player {gameBoard = newBoard, status = PStatus {score = 0, tStatus = Loosing}}
  where
    (Board oldNewCells) = gameBoard player
    newBoard@(Board newCells) =
      Board $
        (\cell -> if n == value cell then cell {isSet = True} else cell) <$> oldNewCells
    player' = player {gameBoard = newBoard}
    canPlayerWin =
      isVictoryOnRows || isVictoryOnCols
      where
        isVictoryOnRows = checkGridLine M.getRow M.nrows
        isVictoryOnCols = checkGridLine M.getCol M.ncols
        checkGridLine f g =
          or $ all isSet <$> filter (any (\c -> value c == n)) (flip f newCells <$> [1 .. (g newCells)])

computeScore :: BingoNumber -> Player -> Score
computeScore n p =
  let (Board cells) = gameBoard p
   in Score $
        n
          * foldr'
            (\cell currentScore -> if isSet cell then currentScore else value cell + currentScore)
            0
            cells

readInputOfDay4 :: MonadIO m => Text -> m (Either AocParserError BingoGame)
readInputOfDay4 path = do
  content <- readFile . toString $ path
  TM.runParserT parseBingoGame "Day 4 / part 1" . toText $ content

parseBingoGame :: AocParserT m BingoGame
parseBingoGame = do
  rawNumbers <- TM.sepBy (L.decimal :: AocParserT m Int) ","
  boards <- TM.manyTill pBoard TM.eof
  return $
    BingoGame
      { bingoNumbers = rawNumbers,
        turn = 1,
        players = (\b -> Player {gameBoard = Board b, status = PStatus {tStatus = Loosing, score = 0}}) <$> boards
      }
  where
    pInt = do
      _ <- TMC.space
      int <- (L.decimal :: AocParserT m Int)
      _ <- TMC.space
      return int

    pBoardLine = TM.count 5 pInt

    pBoard = do
      board <- TM.count 5 pBoardLine
      return $ flip Cell False <$> M.fromLists board

runDay4 :: MonadIO m => m ()
runDay4 = do
  p1 <- computeAnswerOfTheDay @Int (mkPartOne 4) readInputOfDay4 day4Part1
  printAnswerOfTheDay p1

  p2 <- computeAnswerOfTheDay @Int (mkPartTwo 4) readInputOfDay4 day4Part2
  printAnswerOfTheDay p2
