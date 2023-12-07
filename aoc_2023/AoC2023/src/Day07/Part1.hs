{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Day07.Part1 (Bid, Hand (..), Card (..), HandType (..), solution, evaluateHands, determineHandType, frequencyCounts) where

import Data.Attoparsec.Text
import Data.Either (rights)
import Data.List (sortOn)
import qualified Data.Map.Strict as M
import Data.Ord (Down (..))
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

-- Types

data Card = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace deriving (Eq, Show, Enum, Ord)
data HandType = HighCard | OnePair | TwoPair | ThreeOfAKind | FullHouse | FourOfAKind | FiveOfAKind deriving (Eq, Show, Enum, Ord)
newtype Hand = Hand [Card] deriving (Eq, Show)
type Bid = Int

-- Typeclasses & Instances

instance Ord Hand where
    compare (Hand hand1) (Hand hand2) =
        let handType1 = determineHandType hand1
            handType2 = determineHandType hand2
         in case compare handType1 handType2 of
                EQ -> compare (fromEnum <$> hand1) (fromEnum <$> hand2)
                result -> result

-- Functions

solution :: FilePath -> ([(Hand, Bid)] -> Int) -> IO Int
solution filePath valueFunc = do
    contents <- TIO.readFile filePath
    let hands = rights $ parseOnly parseLine <$> T.lines contents
    return $ valueFunc hands

frequencyCounts :: (Ord a) => [a] -> M.Map a Int
frequencyCounts list = M.fromListWith (+) [(x, 1) | x <- list]

determineHandType :: [Card] -> HandType
determineHandType hand
    | head decreasingFreqs == 5 = FiveOfAKind
    | head decreasingFreqs == 4 = FourOfAKind
    | head decreasingFreqs == 3 && decreasingFreqs !! 1 == 2 = FullHouse
    | head decreasingFreqs == 3 = ThreeOfAKind
    | head decreasingFreqs == 2 && decreasingFreqs !! 1 == 2 = TwoPair
    | head decreasingFreqs == 2 = OnePair
    | otherwise = HighCard
  where
    freqsMap = frequencyCounts hand
    decreasingFreqs = sortOn Down $ map snd $ M.toList freqsMap

evaluateHands :: (Ord a) => [(a, Bid)] -> Int
evaluateHands hands =
    let rankedHands = zip [1 ..] $ sortOn fst hands
     in sum $ map (\(rank, (_, bid)) -> rank * bid) rankedHands

-- Parsers

parseLine :: Parser (Hand, Bid)
parseLine = do
    hand <- parseHand
    space
    bid <- decimal
    return (hand, bid)

parseCard :: Parser Card
parseCard = do
    c <- satisfy $ inClass "23456789TJQKA"
    case c of
        '2' -> return Two
        '3' -> return Three
        '4' -> return Four
        '5' -> return Five
        '6' -> return Six
        '7' -> return Seven
        '8' -> return Eight
        '9' -> return Nine
        'T' -> return Ten
        'J' -> return Jack
        'Q' -> return Queen
        'K' -> return King
        'A' -> return Ace
        _ -> fail "Invalid card"

parseHand :: Parser Hand
parseHand = do
    hand <- count 5 parseCard
    return $ Hand hand
