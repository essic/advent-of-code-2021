module HsAoc2021.Day3
  ( day3Part2,
    day3Part1,
    readInputOfDay3,
  )
where

import qualified Data.Matrix as M
import qualified Data.Set as Set
import qualified Data.Vector as V
import HsAoc2021.Types
  ( AocParserT,
  )
import Relude
import Relude.Unsafe ((!!))
import qualified Text.Megaparsec as TM
import qualified Text.Megaparsec.Char as TMC

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

day3Part1 :: DiagnosticReport -> PowerConsumption
day3Part1 (DR x) =
  foldl' (*) 1 $ sumBits <$> [allBitsForGamma, allBitsForEpsilon]
  where
    nbColumns = M.ncols x
    allColumns = [1 .. nbColumns]
    allBitsForGamma = bitWeightForEachColumn $ bitsForRates getMostSignificantBit
    allBitsForEpsilon = bitWeightForEachColumn $ bitsForRates getLeastSignificantBit
    bitsForRates f =
      (\c -> f $ M.getCol c x) <$> allColumns

day3Part2 :: DiagnosticReport -> LifeSupportRating
day3Part2 (DR x) =
  foldl' (*) 1 [oxigenGeneratorRating, co2ScrubberRating]
  where
    oxigenGeneratorRating = sumBits . bitWeightForEachColumn $ execute getMostSignificantBit x 1
    co2ScrubberRating = sumBits . bitWeightForEachColumn $ execute getLeastSignificantBit x 1
    filterOnMatrixRow f = M.fromLists . filter f . M.toLists
    execute f mx y
      | M.nrows mx == 1 =  V.toList $ M.getRow 1 mx
      | otherwise =
        let sbToFind = f $ M.getCol y mx
            mx' = filterOnMatrixRow (\row -> (row !! (y - 1)) == sbToFind) mx
         in execute f mx' (y + 1)


sumBits :: [(Int,Bool)] -> Int
sumBits = sum . fmap (\(weight, value) -> if value then weight else 0)

bitWeightForEachColumn ::  [a] -> [(Int,a)]
bitWeightForEachColumn xs =
  zip ys xs
  where
    nCols = length xs
    ys = (\c -> (2 :: Int) ^ (nCols - c)) <$> [1..nCols]

getMostSignificantBit :: (Functor t, Foldable t) => t Bool -> Bool
getMostSignificantBit bools =
  numbersOfTrue >= numberOfFalse
  where
    getNumberOfElem x =
      sum $ (\a -> if a == x then 1 else (0 :: Int)) <$> bools
    numberOfFalse = getNumberOfElem False
    numbersOfTrue = getNumberOfElem True

getLeastSignificantBit :: (Functor t, Foldable t) => t Bool -> Bool
getLeastSignificantBit = not . getMostSignificantBit

-- Getting input for day 3
readInputOfDay3 :: MonadIO m => Text -> m (Either (TM.ParseErrorBundle Text Void) DiagnosticReport)
readInputOfDay3 path = do
  content <- readFile . toString $ path
  TM.runParserT parseDiagnosticReport "input" . toText $ content

parseDiagnosticReport :: AocParserT m DiagnosticReport
parseDiagnosticReport = do
  rawLines <- parseLines <|> fail "We cannot parse Day 3 input !"
  case mkDiagnosticReport . toBits $ rawLines of
    Just report -> return report
    Nothing -> TM.failure Nothing (Set.fromList [])
  where
    parseLine = TM.manyTill (TM.satisfy (\c -> c == '1' || c == '0') :: AocParserT m Char) TMC.eol
    parseLines = TM.manyTill parseLine TM.eof
    toBits = (fmap . fmap) (== '1')
