module Day13.Part2 (summarizePatterns') where

import qualified Data.Vector as V
import Day13.Part1

summarizePatterns' :: [Pattern] -> Int
summarizePatterns' = sum . map summarizePattern'

summarizePattern' :: Pattern -> Int
summarizePattern' pattern = sum ((+ 1) <$> vReflecting) + (100 * sum ((+ 1) <$> hReflecting))
  where
    (hPattern, vPattern) = pairHorizontalVerticalPatterns pattern
    hReflecting = findReflectIndexExactlyOneMismatch hPattern
    vReflecting = findReflectIndexExactlyOneMismatch vPattern

findReflectIndexExactlyOneMismatch :: VecPattern -> [Index]
findReflectIndexExactlyOneMismatch pattern = filter (isReflectingIndex' pattern) [0 .. V.length pattern - 1]

isReflectingIndex' :: VecPattern -> Index -> Bool
isReflectingIndex' pattern idx = sum [countDiffs i j | (i, j) <- genMatchingIndices idx, i < length pattern, j < length pattern] == 1
  where
    countDiffs i j = length $ V.filter (uncurry (/=)) (V.zip (pattern V.! i) (pattern V.! j))
