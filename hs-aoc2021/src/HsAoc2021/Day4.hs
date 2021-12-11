module HsAoc2021.Day4
  ( day4Part1,
    readInputOfDay4,
    day4Part2,
  )
where

import Data.Either.Combinators (mapLeft)
import Data.Foldable (foldr')
import qualified Data.Matrix as M
import HsAoc2021.Types
  ( AocParserT,
    BingoBoard (..),
    BingoCell (..),
    BingoGame (..),
    BingoPlayer (..),
    BingoPlayerStatus (..),
    BingoRound (..),
    BingoScore,
  )
import Relude
  ( Bool (..),
    Either (Left, Right),
    Eq ((/=), (==)),
    Int,
    Maybe (Just, Nothing),
    Monad (return),
    MonadIO,
    Num ((*), (+)),
    Text,
    Void,
    all,
    any,
    compare,
    const,
    filter,
    flip,
    fromRight,
    length,
    not,
    or,
    otherwise,
    readFile,
    snd,
    toString,
    toText,
    ($),
    (&&),
    (-),
    (.),
    (<$>),
    (>),
    (||),
  )
import Relude.Debug (traceShowId, traceShowWith)
import Relude.Unsafe ((!!))
import Safe (maximumByMay)
import qualified Text.Megaparsec as TM
import qualified Text.Megaparsec.Char as TMC
import qualified Text.Megaparsec.Char.Lexer as L

day4Part1 :: BingoGame -> BingoScore
day4Part1 g =
  fromRight (-100) (playTurnTillFirstWin g)

day4Part2 :: BingoGame -> BingoScore
day4Part2 g =
  fromRight (-100) (playTurnTillLastWin g)

playTurnTillLastWin :: BingoGame -> Either Text Int
playTurnTillLastWin game =
  case findNextWinner game of
    Left err -> Left err 
    Right (game', w) ->
      case playersLeft of
        [] -> Right .  score $ w
        _ ->  playTurnTillLastWin $ game' {players = playersLeft}
      where
        playersLeft = filter (\p -> gameBoard p /= gameBoard w) (players game')

playTurnTillFirstWin :: BingoGame -> Either Text Int
playTurnTillFirstWin game =
  mapLeft (const "We did not find a winner !") $ score . snd <$> findNextWinner game

findNextWinner :: BingoGame -> Either Text (BingoGame, BingoPlayer)
findNextWinner game =
  if turn > maxNumberOfTurn
    then Left "We failed ! We cannot outdone the range of available bingo numbers !"
    else case winnerMay of
      Nothing -> findNextWinner $ game {players = turnPlayed, round = Round (turn + 1)}
      Just winner ->
        Right
          ( game {players = setResults (score winner) <$> turnPlayed},
            winner {status = Won}
          )
  where
    turn = getRoundValue . round $ game
    maxNumberOfTurn = length $ numbersToMatch game
    bingoNumber = numbersToMatch game !! (turn - 1)
    turnPlayed = playTurn bingoNumber <$> players game
    winnerMay =
      maximumByMay (\p1 p2 -> score p1 `compare` score p2) $
        filter (\x -> status x == MayWin) turnPlayed
    setResults score' player'
      | score player' == score' && status player' == MayWin = player' {status = Won}
      | status player' /= Won = player' {status = Loosing}
      | otherwise = player'

getRoundValue :: BingoRound -> Int
getRoundValue x =
  case x of
    NoRoundPlayed -> 1
    Round r -> r

playTurn :: Int -> BingoPlayer -> BingoPlayer
playTurn n player =
  let player' = markBoard n player
      board' = gameBoard player'
   in if checkVictoryCondition n . gameBoard $ player'
        then player' {score = calculateScore n board', status = MayWin}
        else player' {score = calculateScore n board', status = Loosing}

markBoard :: Int -> BingoPlayer -> BingoPlayer
markBoard n player =
  player {gameBoard = newBoard}
  where
    (BingoBoard cells) = gameBoard player
    newBoard = BingoBoard $ (\(BingoCell o@(n', _)) -> BingoCell $ if n' == n then (n', True) else o) <$> cells

checkVictoryCondition :: Int -> BingoBoard -> Bool
checkVictoryCondition n (BingoBoard cells) =
  rowsVictory || colsVictory
  where
    allRows = flip M.getRow cells <$> [1 .. (M.nrows cells)]
    allCols = flip M.getCol cells <$> [1 .. (M.ncols cells)]
    onlyRowsWithTheNumber = filter (any (\(BingoCell (c, _)) -> c == n)) allRows
    onlyColsWithTheNumber = filter (any (\(BingoCell (c, _)) -> c == n)) allCols
    rowsVictory = or $ all (\(BingoCell (_, s)) -> s) <$> onlyRowsWithTheNumber
    colsVictory = or $ all (\(BingoCell (_, s)) -> s) <$> onlyColsWithTheNumber

calculateScore :: Int -> BingoBoard -> Int
calculateScore n (BingoBoard cells) =
  n * foldr' (\(BingoCell (v, s)) c -> if not s then v + c else c) 0 cells

readInputOfDay4 :: MonadIO m => Text -> m (Either (TM.ParseErrorBundle Text Void) BingoGame)
readInputOfDay4 path = do
  content <- readFile . toString $ path
  TM.runParserT parseBingoGame "Day 4 / part 1" . toText $ content

parseBingoGame :: AocParserT m BingoGame
parseBingoGame = do
  bingoNumbers <- TM.sepBy (L.decimal :: AocParserT m Int) ","
  boards <- TM.manyTill pBoard TM.eof
  return $
    BingoGame
      { numbersToMatch = bingoNumbers,
        round = NoRoundPlayed,
        players = (\b -> BingoPlayer {gameBoard = BingoBoard b, score = 0, status = Loosing}) <$> boards
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
      return $ (\i -> BingoCell (i, False)) <$> M.fromLists board
