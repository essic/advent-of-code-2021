{-# LANGUAGE DerivingStrategies #-}

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
    all,
    head,
    nonEmpty,
    ($),
    (.),
    (<$>),
  )

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
