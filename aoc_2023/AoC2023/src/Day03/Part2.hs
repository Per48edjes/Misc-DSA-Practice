{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Day03.Part2 (sumGearRatios) where

import qualified Data.Set as S
import Day03.Part1 (Entity (..), getSurroundingCoords, partitionEntities)

sumGearRatios :: [[Entity]] -> Int
sumGearRatios entities = sum $ map (\ps -> if length ps == 2 then product ps else 0) groupedGears
  where
    (partNums, symbols) = partitionEntities entities
    groupedGears = [[partNum.entityValue | partNum <- partNums, isAdjacent gear partNum] | gear <- filter isGear symbols]

isAdjacent :: Entity -> Entity -> Bool
isAdjacent e1 e2 = (not . S.null) $ S.intersection (getSurroundingCoords e1) e2.entityCoords

isGear :: Entity -> Bool
isGear (Symbol _ "*") = True
isGear _ = False
