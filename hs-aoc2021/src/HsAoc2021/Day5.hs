{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module HsAoc2021.Day5 (runDay5) where

import Data.Char (isDigit)
import qualified Data.Map.Strict as Map
import HsAoc2021.Types
  ( AocParserError,
    AocParserT,
    computeAnswerOfTheDay,
    mkPartOne,
    mkPartTwo,
    printAnswerOfTheDay,
  )
import Relude
import qualified Text.Megaparsec as TM
import qualified Text.Megaparsec.Char as TMC
import qualified Text.Megaparsec.Char.Lexer as L

newtype PuzzleLine = PI (Point, Point)
  deriving stock (Show)

data Point = Pt
  { posX :: Int,
    posY :: Int
  }
  deriving stock (Eq, Show, Ord)

data Line
  = LHorizontal [Point]
  | LVertical [Point]
  | LDiagonalAt45 [Point]
  | LDiagonal [Point]

day5Part1 :: [PuzzleLine] -> Either Text Int
day5Part1 input =
  Right $ execute input (Map.empty @Point @Int)
  where
    execute [] accMap = howManyDangerousOverlaps accMap
    execute (PI (p1, p2) : ls) accMap =
      case mkLine p1 p2 of
        (LHorizontal pts) -> execute ls $ markIntersections pts accMap
        (LVertical pts) -> execute ls $ markIntersections pts accMap
        _ -> execute ls accMap

day5Part2 :: [PuzzleLine] -> Either Text Int
day5Part2 input =
  Right $ execute input (Map.empty @Point @Int)
  where
    execute [] accMap = howManyDangerousOverlaps accMap
    execute (PI (p1, p2) : ls) accMap =
      case mkLine p1 p2 of
        (LHorizontal pts) -> execute ls $ markIntersections pts accMap
        (LVertical pts) -> execute ls $ markIntersections pts accMap
        (LDiagonalAt45 pts) -> execute ls $ markIntersections pts accMap
        _ -> execute ls accMap

mkPoint :: (Int, Int) -> Point
mkPoint (x, y) =
  Pt {posX = x, posY = y}

mkLine :: Point -> Point -> Line
mkLine p1 p2
  | x1 == x2 = LHorizontal $ mkPoint . (x1,) <$> range y1 y2
  | y1 == y2 = LVertical $ mkPoint . (,y1) <$> range x1 x2
  | angleInDegrees == 45.0 = LDiagonalAt45 $ mkPoint <$> zip (range x1 x2) (range y1 y2)
  | otherwise = LDiagonal [p1, p2]
  where
    (x1, y1) = (posX p1, posY p1)
    (x2, y2) = (posX p2, posY p2)
    dy = fromIntegral (y2 - y1) :: Float
    dx = fromIntegral (x2 - x1) :: Float
    t = atan (dy / dx)
    angleInDegrees = abs $ t * 180 / pi -- We do not care about the orientation of the angle.
    range a b =
      case [a .. b] of
        [] -> reverse [b .. a]
        r -> r

markIntersections :: (Ord k, Num a) => [k] -> Map k a -> Map k a
markIntersections [] m' = m'
markIntersections (p : ps) m' = markIntersections ps $ Map.alter alterFunc p m'
  where
    alterFunc Nothing = Just 1
    alterFunc r = (+ 1) <$> r

howManyDangerousOverlaps :: Map Point Int -> Int
howManyDangerousOverlaps xs = length $ Map.filter (>= 2) xs

readInputOfDay5 :: MonadIO m => Text -> m (Either AocParserError [PuzzleLine])
readInputOfDay5 path = do
  content <- readFile . toString $ path
  TM.runParserT parsePuzzleInput "Day 5" . toText $ content

parsePuzzleInput :: AocParserT m [PuzzleLine]
parsePuzzleInput = TM.manyTill parseLine TM.eof
  where
    parseLine = do
      coord1 <- TM.sepBy (L.decimal :: AocParserT m Int) ","
      _ <- TM.takeWhileP Nothing (not . isDigit)
      coord2 <- TM.sepBy (L.decimal :: AocParserT m Int) ","
      _ <- TMC.space
      case (coord1, coord2) of
        ([pX1, pY1], [pX2, pY2]) -> return . PI $ (mkPoint (pX1, pY1), mkPoint (pX2, pY2))
        _ -> fail $ "Cannot parse coordinates !" <> show coord1 <> " and " <> show coord2

runDay5 :: (MonadIO m) => m ()
runDay5 = do
  p1 <- computeAnswerOfTheDay (mkPartOne @Text @Int 5) readInputOfDay5 day5Part1
  printAnswerOfTheDay p1

  p2 <- computeAnswerOfTheDay (mkPartTwo @Text @Int 5) readInputOfDay5 day5Part2
  printAnswerOfTheDay p2
