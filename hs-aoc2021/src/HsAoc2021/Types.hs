{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module HsAoc2021.Types
  ( Depth,
    Speed,
    Direction (..),
    Command (..),
    Position (..),
    PowerConsumption,
    DiagnosticReport (..),
    LifeSupportRating,
    mkDiagnosticReport,
    AocParserT,
    BingoRound (..),
    BingoScore,
    BingoBoard (..),
    BingoCell (..),
    BingoGame (..),
    BingoPlayer (..),
    BingoPlayerStatus (..)
  )
where

import qualified Data.Matrix as M
import Relude
  ( Bool,
    Eq ((==)),
    Foldable (length),
    Int,
    Maybe (..),
    Show,
    Text,
    Void,
    all,
    head,
    nonEmpty,
    ($),
    (.),
    (<$>),
  )
import qualified Text.Megaparsec as TM

type AocParserT = TM.ParsecT Void Text

type Speed = Int

type Depth = Int

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

newtype DiagnosticReport = DR (M.Matrix Bool)

mkDiagnosticReport :: [[Bool]] -> Maybe DiagnosticReport
mkDiagnosticReport xs =
  case nonEmpty xs of
    Nothing -> Nothing
    Just xs' ->
      let rowsLength = length <$> xs'
          l = head rowsLength
       in if all (== l) rowsLength then Just . DR . M.fromLists $ xs else Nothing

type PowerConsumption = Int

type LifeSupportRating = Int

type BingoScore = Int

newtype BingoCell = BingoCell (Int, Bool)
  deriving stock (Show,Eq)

newtype BingoBoard = BingoBoard (M.Matrix BingoCell)
  deriving stock (Show,Eq)

data BingoPlayerStatus = Loosing | Won | MayWin
  deriving stock (Show, Eq)

data BingoPlayer = BingoPlayer
  {
    gameBoard :: BingoBoard,
    score :: BingoScore,
    status :: BingoPlayerStatus
  }
  deriving stock (Show)

data BingoRound = NoRoundPlayed | Round Int
  deriving stock (Show)

data BingoGame = BingoGame
  { round :: BingoRound,
    numbersToMatch :: [Int],
    players :: [BingoPlayer]
  }
  deriving stock (Show)
