{-# LANGUAGE OverloadedStrings #-}

module Day04.Part1 (solution, scoreCards, Card, GameNum, getMatches) where

import Data.Attoparsec.Text
import Data.Char (isDigit)
import Data.Either (rights)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

type Card = (GameNum, [CardNum], [WinNum])
type GameNum = Int
type CardNum = Int
type WinNum = Int

solution :: FilePath -> ([Card] -> Int) -> IO Int
solution filePath valueFunc = do
    contents <- TIO.readFile filePath
    let parsed = rights $ parseOnly parseCard <$> T.lines contents
    return $ valueFunc parsed

scoreCards :: [Card] -> Int
scoreCards = sum . map scoreCard
  where
    scoreCard card =
        let matches = getMatches card
         in if matches == 0 then 0 else 2 ^ (matches - 1)

getMatches :: Card -> Int
getMatches (_, cardNums, winNums) = S.size $ S.fromList cardNums `S.intersection` S.fromList winNums

-- Parsers

parseCard :: Parser Card
parseCard = do
    gameNum <- parseGameNum
    cardNums <- parseCardNums
    winNums <- parseWinNums
    return (gameNum, cardNums, winNums)

parseGameNum :: Parser GameNum
parseGameNum = skipWhile (not . isDigit) *> decimal <* char ':'

parseCardNums :: Parser [CardNum]
parseCardNums = manyTill' (many' space *> decimal) (string " |")

parseWinNums :: Parser [WinNum]
parseWinNums = manyTill' (many' space *> decimal) endOfInput
