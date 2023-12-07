module Day06.Part1 (solution, marginOfError, RaceRecord) where

import Control.Applicative ((<|>))
import Data.Attoparsec.Text
import Data.Char (isSpace)
import qualified Data.Text.IO as TIO

type RaceRecord = (Int, Int)

solution :: FilePath -> ([RaceRecord] -> Int) -> IO Int
solution filePath valueFunc = do
    contents <- TIO.readFile filePath
    let raceRecords = case parseOnly raceRecordsParser contents of
            Left err -> error err
            Right records -> records
    return $ valueFunc raceRecords

marginOfError :: [RaceRecord] -> Int
marginOfError = product . map waysToBeatRecord

waysToBeatRecord :: RaceRecord -> Int
waysToBeatRecord (t', d) = ceiling r2 - floor r1 - 1
  where
    t = fromIntegral t' :: Double
    (r2, r1) = ((t + sqrt (t ** 2 - 4 * fromIntegral d)) / 2, (t - sqrt (t ** 2 - 4 * fromIntegral d)) / 2)

-- Parsers

raceRecordsParser :: Parser [RaceRecord]
raceRecordsParser = do
    times <- valuesParser
    distances <- valuesParser
    return $ zip times distances

valuesParser :: Parser [Int]
valuesParser = do
    _ <- (string "Time: " <|> string "Distance: ")
    manyTill (skipWhile isSpace *> decimal) (endOfLine <|> endOfInput)
