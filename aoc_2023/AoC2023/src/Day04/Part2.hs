module Day04.Part2 (countCards) where

import Control.Monad.State
import qualified Data.HashMap.Strict as M
import Data.Maybe (fromMaybe)
import Day04.Part1 (Card, GameNum, getMatches)

type CardCounter = M.HashMap GameNum Int
type CardCount = Int

countCards :: [Card] -> Int
countCards cards =
    let (totalCards, _) = execState (mapM_ tallyCard cards) (0, initialCardCounter)
     in totalCards
  where
    initialCardCounter = M.fromList [(gameNum, 1) | (gameNum, _, _) <- cards]
    tallyCard :: Card -> State (CardCount, CardCounter) ()
    tallyCard (gameNum, _, _) = do
        (cumCardCount, cardCounter) <- get
        let currentCardCount = fromMaybe 0 $ gameNum `M.lookup` cardCounter
            subsequentCards = [gameNum + i | i <- [1 .. getMatches (cards !! (gameNum - 1))]]
            cardCounter' = foldr (M.adjust (+ currentCardCount)) cardCounter subsequentCards
        put (cumCardCount + currentCardCount, cardCounter')
