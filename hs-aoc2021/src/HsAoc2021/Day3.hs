module HsAoc2021.Day3 (day3Part2, day3Part1, readInputOfDay3) where

import qualified Data.Matrix as M
import qualified Data.Set as Set
import qualified Data.Vector as V
import HsAoc2021.Types
  ( DiagnosticReport (..),
    LifeSupportRating,
    PowerConsumption,
    mkDiagnosticReport,
  )
import Relude
  ( Bool (..),
    Char,
    Either,
    Eq ((==)),
    Foldable (foldl'),
    Functor (fmap),
    Int,
    Maybe (..),
    MonadIO,
    Num ((*), (+), (-)),
    Ord ((>=)),
    Text,
    Void,
    fail,
    filter,
    not,
    otherwise,
    readFile,
    return,
    sum,
    toString,
    toText,
    zip,
    ($),
    (.),
    (<$>),
    (<|>),
    (^),
    (||),
  )
import Relude.Unsafe ((!!))
import qualified Text.Megaparsec as TM
import qualified Text.Megaparsec.Char as TMC

day3Part1 :: DiagnosticReport -> PowerConsumption
day3Part1 (DR x) =
  foldl' (*) 1 $ sumBits <$> [allBitsForGamma, allBitsForEpsilon]
  where
    nbColumns = M.ncols x
    allColumns = [1 .. nbColumns]
    allBitsForGamma = (\c -> getMostSignificantBit $ M.getCol c x) <$> allColumns
    allBitsForEpsilon = (\c -> getLeastSignificantBit $ M.getCol c x) <$> allColumns
    binaryWeightedColumns = (\c -> (2 :: Int) ^ (nbColumns - c)) <$> allColumns
    sumBits = sum . fmap (\(weight, value) -> if value then weight else 0) . zip binaryWeightedColumns

day3Part2 :: DiagnosticReport -> LifeSupportRating
day3Part2 (DR x) =
  foldl' (*) 1 [oxigenGeneratorRating, co2ScrubberRating]
  where
    oxigenGeneratorRating = sumBits . V.toList . M.getRow 1 $ execute getMostSignificantBit x 1
    co2ScrubberRating = sumBits . V.toList . M.getRow 1 $ execute getLeastSignificantBit x 1
    nbColumns = M.ncols x
    binaryWeightedColumns = (\c -> (2 :: Int) ^ (nbColumns - c)) <$> [1 .. nbColumns]
    sumBits = sum . fmap (\(weight, value) -> if value then weight else 0) . zip binaryWeightedColumns
    filterMatrixByRow f = M.fromLists . filter f . M.toLists
    execute f mx y
      | M.nrows mx == 1 = mx
      | otherwise =
        let sbToFind = f $ M.getCol y mx
            mx' = filterMatrixByRow (\row -> (row !! (y - 1)) == sbToFind) mx
         in execute f mx' (y + 1)

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

type Day3InputParserT = TM.ParsecT Void Text

parseDiagnosticReport :: Day3InputParserT m DiagnosticReport
parseDiagnosticReport = do
  rawLines <- parseLines <|> fail "We cannot parse Day 3 input !"
  case mkDiagnosticReport . toBits $ rawLines of
    Just report -> return report
    Nothing -> TM.failure Nothing (Set.fromList [])
  where
    parseLine = TM.manyTill (TM.satisfy (\c -> c == '1' || c == '0') :: Day3InputParserT m Char) TMC.eol
    parseLines = TM.manyTill parseLine TM.eof
    toBits = (fmap . fmap) (== '1')

