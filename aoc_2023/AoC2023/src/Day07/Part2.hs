module Day07.Part2 (evaluateHands') where

import Data.Bifunctor (first)
import qualified Data.Map.Strict as M
import Day07.Part1 (Bid, Card (..), Hand (..), HandType (..), determineHandType, evaluateHands, frequencyCounts)

-- Types

data WCard = WJack | WTwo | WThree | WFour | WFive | WSix | WSeven | WEight | WNine | WTen | WQueen | WKing | WAce deriving (Eq, Show, Enum, Ord)
newtype WHand = WHand [WCard] deriving (Eq, Show)

-- Typeclasses & Instances

instance Ord WHand where
    compare (WHand hand1) (WHand hand2) =
        let handType1 = determineHandType' hand1
            handType2 = determineHandType' hand2
         in case compare handType1 handType2 of
                EQ -> compare (fromEnum <$> hand1) (fromEnum <$> hand2)
                result -> result

-- Functions

cardToWCard :: Card -> WCard
cardToWCard Jack = WJack
cardToWCard card = toEnum $ fromEnum card + (if card < Queen then 1 else 0)

wCardToCard :: WCard -> Card
wCardToCard WJack = Jack
wCardToCard wCard = toEnum $ fromEnum wCard - (if wCard < WQueen then 1 else 0)

handToWHand :: Hand -> WHand
handToWHand (Hand hand) = WHand $ cardToWCard <$> hand

evaluateHands' :: [(Hand, Bid)] -> Int
evaluateHands' hands = evaluateHands wHands
  where
    wHands = map (first handToWHand) hands

determineHandType' :: [WCard] -> HandType
determineHandType' wHand
    | WJack `notElem` wHand = defaultHandType
    | otherwise = case defaultHandType of
        FiveOfAKind -> FiveOfAKind
        FourOfAKind -> FiveOfAKind
        FullHouse -> FiveOfAKind
        ThreeOfAKind -> FourOfAKind
        TwoPair
            | freqsMap M.! WJack == 2 -> FourOfAKind
            | otherwise -> FullHouse
        OnePair -> ThreeOfAKind
        HighCard -> OnePair
  where
    freqsMap = frequencyCounts wHand
    defaultHandType = determineHandType $ wCardToCard <$> wHand
