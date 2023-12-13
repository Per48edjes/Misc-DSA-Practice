module Day11.Part2 (sumGalaxyAPSPs') where

import qualified Data.List as L
import qualified Data.Vector as V
import Day11.Part1
import Util (pairwiseCombinations)

expansionFactor :: Int
expansionFactor = 1000000

sumGalaxyAPSPs' :: Grid -> Int
sumGalaxyAPSPs' grid = sum $ map (\((_, c1), (_, c2)) -> calculateSP c1 c2) galaxyPairs
  where
    galaxyPairs = pairwiseCombinations . map (transformNode empties) . collectGalaxies . augmentGrid . transformGrid' $ grid
    empties = getEmptyRowsAndCols grid

transformNode :: ([Index], [Index]) -> (Node Int, Coord) -> (Node Int, Coord)
transformNode (eRs, eCs) (node, (x, y)) = (node, (x + (n * (expansionFactor - 1)), y + (m * (expansionFactor - 1))))
  where
    n = length $ takeWhile (< x) eRs
    m = length $ takeWhile (< y) eCs

getEmptyRowsAndCols :: Grid -> ([Index], [Index])
getEmptyRowsAndCols grid = (rowIdx, colIdx)
  where
    rowIdx = L.sort $ findEmptyRows grid
    colIdx = L.sort $ findEmptyRows (L.transpose grid)

transformGrid' :: Grid -> ExpandedGrid
transformGrid' grid = V.fromList $ map V.fromList grid
