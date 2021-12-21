{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module HsAoc2021.Day5 (runDay5) where

import Data.Char (isDigit)
import qualified Data.Map.Strict as Map
import HsAoc2021.Types (AocParserError, AocParserT, computeAnswerOfTheDay, mkPartOne, mkPartTwo, printAnswerOfTheDay)
import Relude
import qualified Text.Megaparsec as TM
import qualified Text.Megaparsec.Char as TMC
import qualified Text.Megaparsec.Char.Lexer as L

newtype PuzzleLine = PI (Point, Point)
  deriving stock (Show)

readInputOfDay5 :: MonadIO m => Text -> m (Either AocParserError [PuzzleLine])
readInputOfDay5 path = do
  content <- readFile . toString $ path
  TM.runParserT parsePuzzleInput "Day 5" . toText $ content

parsePuzzleInput :: AocParserT m [PuzzleLine]
parsePuzzleInput = do
  TM.manyTill parseLine TM.eof
  where
    parseLine = do
      coord1 <- TM.sepBy (L.decimal :: AocParserT m Int) ","
      _ <- TM.takeWhileP Nothing (not . isDigit)
      coord2 <- TM.sepBy (L.decimal :: AocParserT m Int) ","
      _ <- TMC.space
      case (coord1, coord2) of
        ([x1, y1], [x2, y2]) -> return . PI $ (mkPoint (x1, y1), mkPoint (x2, y2))
        _ -> fail $ "Cannot parse coordinates !" <> show coord1 <> " and " <> show coord2

data Point = Pt
  { x :: Int,
    y :: Int
  }
  deriving stock (Eq, Show, Ord)

mkPoint :: (Int, Int) -> Point
mkPoint (x', y') =
  Pt {x = x', y = y'}

data Line = LHorizontal [Point] | LVertical [Point] | LDiagonalAt45 [Point] | LDiagonal [Point]

computeAllPointsOfLine :: Point -> Point -> Line
computeAllPointsOfLine p1 p2
  | x p1 == x p2 = LHorizontal $ mkPoint . (x p1,) <$> range (y p1) (y p2)
  | y p1 == y p2 = LVertical $ mkPoint . (,y p1) <$> range (x p1) (x p2)
  | otherwise =
      if angle /= 45.0 then LDiagonal [p1,p2]
      else
       LDiagonalAt45 $ mkPoint <$> zip ( range (x p1) (x p2) ) ( range (y p1) (y p2) )
  where
    angle =
      let dy = (fromIntegral (y p2 - y p1) :: Float)
          dx = (fromIntegral (x p2 - x p1) :: Float)
          t  = atan (dy/dx)
          in abs $ t * 180 / pi
    range a b =
      let r = [a..b]
       in case r of
          [] -> reverse [b..a]
          _  -> r

markIntersections :: (Ord k, Num a) => [k] -> Map k a -> Map k a
markIntersections [] m' = m'
markIntersections (p : ps) m' = markIntersections ps $ Map.alter alterFunc p m'
  where
    alterFunc Nothing =Just 1
    alterFunc r = (+ 1) <$> r

howManyDangerousOverlaps :: Map Point Int -> Int
howManyDangerousOverlaps xs = length $ Map.filter (>=2) xs

day5Part1 :: [PuzzleLine] -> Either Text Int
day5Part1 input =
  execute input (Map.empty @Point @Int)
  where
    execute [] mPoints = Right . howManyDangerousOverlaps $ mPoints
    execute (PI (p1, p2) : ls) mPoints =
      case computeAllPointsOfLine p1 p2 of
        (LHorizontal pts) -> execute ls $ markIntersections pts mPoints
        (LVertical pts) -> execute ls $ markIntersections pts mPoints
        _ -> execute ls mPoints

day5Part2 :: [PuzzleLine] -> Either Text Int
day5Part2 input =
  execute input (Map.empty @Point @Int)
  where
    execute [] mPoints = Right . howManyDangerousOverlaps $ mPoints
    execute (PI (p1, p2) : ls) mPoints =
      case computeAllPointsOfLine p1 p2 of
        (LHorizontal pts) -> execute ls $ markIntersections pts mPoints
        (LVertical pts) -> execute ls $ markIntersections pts mPoints
        (LDiagonalAt45 pts) -> execute ls $ markIntersections pts mPoints
        _ -> execute ls mPoints

runDay5 :: (MonadIO m) => m ()
runDay5 = do
  p1 <- computeAnswerOfTheDay (mkPartOne @Text @Int 5) readInputOfDay5 day5Part1
  printAnswerOfTheDay p1

  p2 <- computeAnswerOfTheDay (mkPartTwo @Text @Int 5) readInputOfDay5 day5Part2
  printAnswerOfTheDay p2
