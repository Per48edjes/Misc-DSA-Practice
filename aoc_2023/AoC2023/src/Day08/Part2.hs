module Day08.Part2 (countMoves') where

import Control.Monad.Extra
import Control.Monad.State
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Text as T
import Day08.Part1 (Move, Node, processMove)

findStartNodes :: [(Node, (Node, Node))] -> [Node]
findStartNodes nodeAdjacencies = map fst $ filter (\(n, _) -> T.last n == 'A') nodeAdjacencies

countMoves' :: [Move] -> [(Node, (Node, Node))] -> Int
countMoves' moveSequence nodeAdjacencies = dStar * product cycleLengths'
  where
    dStar = length moveSequence
    cycleLengths = map (findCycleLength moveSequence nodeAdjacencies) $ findStartNodes nodeAdjacencies
    cycleLengths' = map (`div` dStar) cycleLengths

-- Helper functions (to investigate the input data)

findCycleLength :: [Move] -> [(Node, (Node, Node))] -> Node -> Int
findCycleLength moveSequence nodeAdjacencies startNode =
    let (_, cycleLength) = execState (anyM (processMove ((== 'Z') . T.last) adjacencyMap) $ cycle moveSequence) (startNode, 0)
     in cycleLength
  where
    adjacencyMap = M.fromList nodeAdjacencies

doesCRTApply :: [Int] -> Bool
doesCRTApply xs = all (\(a, b) -> gcd a b == 1) (pairs xs) || (haveSameGCD xs && doesCRTApply ((`div` (head $ pairGCDs xs)) <$> xs))

pairs :: [a] -> [(a, a)]
pairs xs = [(x, y) | (x : ys) <- L.tails xs, y <- ys]

pairGCDs :: [Int] -> [Int]
pairGCDs xs = map (uncurry gcd) $ pairs xs

haveSameGCD :: [Int] -> Bool
haveSameGCD xs = and $ zipWith (==) (pairGCDs xs) (tail $ pairGCDs xs)
