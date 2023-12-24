module Util where

import Data.List (groupBy, subsequences, tails)
import Data.Maybe (isNothing)
import Data.Text (Text)
import qualified Data.Text as T

-- Find the longest run of a specific character in a Text
longestRun :: Char -> Text -> Int
longestRun queryChar text = go 0 0 (T.unpack text)
  where
    go maxRun currentRun [] = max maxRun currentRun
    go maxRun currentRun (x : xs)
        | x == queryChar = go maxRun (currentRun + 1) xs
        | otherwise = go (max maxRun currentRun) 0 xs

-- | Finds all indices of a character in a Text
findIndicesOfChar :: Char -> T.Text -> [Int]
findIndicesOfChar queryChar text = [i | (c, i) <- zip (T.unpack text) [0 ..], c == queryChar]

-- | Replaces characters at given indices in a Text with a new character
replaceAt :: Text -> Char -> [Int] -> Text
replaceAt text newChar indices = T.pack $ replaceAt' (zip [0 ..] (T.unpack text))
  where
    replaceAt' [] = []
    replaceAt' ((i, c) : xs)
        | i `elem` indices = newChar : replaceAt' xs
        | otherwise = c : replaceAt' xs

-- | Generate all sequential partitions of a list into a fixed number of buckets
sequentialPartitions :: [a] -> Int -> [[[a]]]
sequentialPartitions [] n = [replicate n []]
sequentialPartitions _ 0 = [[]]
sequentialPartitions xs 1 = [[xs]]
sequentialPartitions (x : xs) n =
    concat [map (take i (x : xs) :) (sequentialPartitions (drop i (x : xs)) (n - 1)) | i <- [0 .. length (x : xs)]]

-- | Get all pairwise combinations of a list
pairwiseCombinations :: [a] -> [(a, a)]
pairwiseCombinations xs = [(x, y) | (x : ys) <- tails xs, y <- ys]

-- | Generate all combinations of length k from a list
combinationsOf :: Int -> [a] -> [[a]]
combinationsOf k xs = filter ((k ==) . length) $ subsequences xs

-- | Swap elements of a pair
swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)

-- | Determines entry point index of cycle and the cycle length (if it exists) in a linked list
floydTortoiseAndHare :: (Eq a) => [a] -> Maybe (Int, Int)
floydTortoiseAndHare [] = Nothing
floydTortoiseAndHare [_] = Nothing
floydTortoiseAndHare [x, y] = if x == y then Just (0, 1) else Nothing
floydTortoiseAndHare xs = go 0 0
  where
    xs' = zip [0 ..] xs
    go t h
        | isNothing (h `lookup` xs') = Nothing
        | t /= h && t `lookup` xs' == h `lookup` xs' = go' 0 h
        | otherwise = go (t + 1) (h + 2)
    go' t h
        | isNothing (h `lookup` xs') = Nothing
        | t `lookup` xs' == h `lookup` xs' = go'' t (t + 1)
        | otherwise = go' (t + 1) (h + 1)
    go'' t h
        | isNothing (h `lookup` xs') = Nothing
        | t `lookup` xs' == h `lookup` xs' = Just (t, h - t)
        | otherwise = go'' t (h + 1)
