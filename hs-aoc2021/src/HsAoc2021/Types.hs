{-# LANGUAGE DerivingStrategies #-}
module HsAoc2021.Types
  ( AocParserT,
    traceShowId,
    traceShowWith,
    traceShowM,
    traceShow,
    AocParserError,
    computeAnswerOfTheDay,
    printAnswerOfTheDay,
    runWrapper,
    mkPartOne,
    mkPartTwo
  )
where

import Relude
import qualified Text.Megaparsec as TM
import Text.Show ()

type AocParserT = TM.ParsecT Void Text
type AocParserError = TM.ParseErrorBundle Text Void

data (ToText a) => AocError a = AocInputError Text | AocComputeError a

instance (ToText a) => ToText (AocError a) where
  toText (AocInputError msg ) =
    "An error occured during input parsing : " <> msg

  toText (AocComputeError err) =
     "An error occured during computation " <> toText err

data (ToText a, Num b) => AocContext a b = AocCtx
  { day :: Int,
    part :: Int,
    result :: Maybe (Either (AocError a) b)
  }

instance (ToText a, Show b, Num b) => ToText (AocContext a b) where
  toText ctx =
    case result ctx of
      Nothing -> "No answer yet for day " <> dayTxt <> " / part " <> partTxt <> "!"
      Just r ->
        case r of
          Left y -> "Day " <> dayTxt <> " / part " <> partTxt <> " -> " <> toText y
          Right answer -> "Day " <> dayTxt <> " / part " <> partTxt <> " -> " <> show answer
    where
      dayTxt = show $ day ctx
      partTxt = show $ part ctx

computeAnswerOfTheDay :: (Num b, MonadIO m, ToText a) =>
                         AocContext a b ->
                         (Text -> m (Either i c)) ->
                         (c -> Either a d) ->
                          m (AocContext a d)
computeAnswerOfTheDay ctx readInput compute = do
  inputEither <- readInput $ mconcat ["../data/", "day", dayTxt, ".txt" ]
  return $
    case inputEither of
      Left _ -> ctx { result = Just . Left $ AocInputError "Error during parsing of data" }
      Right input ->
        case compute input of
          Left errCompute -> ctx { result = Just . Left . AocComputeError $ errCompute }
          Right successCompute -> ctx { result = Just . Right $ successCompute }
    where
      dayTxt = show $ day ctx

printAnswerOfTheDay :: (MonadIO m, Show b, Num b, ToText a) => AocContext a b -> m()
printAnswerOfTheDay = putTextLn . toText

runWrapper ::  ( a -> b ) -> a -> Either Text b
runWrapper f x =
  Right $ f x

mkPartOne :: (ToText a, Num b) =>  Int -> AocContext a b
mkPartOne x =
  AocCtx { day = x, part = 1, result = Nothing }

mkPartTwo :: (ToText a, Num b) => Int -> AocContext a b
mkPartTwo x =
  AocCtx { day = x, part = 2, result = Nothing }
