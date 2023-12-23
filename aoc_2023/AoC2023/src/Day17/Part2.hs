module Day17.Part2 where

import qualified Data.Array as A
import Day10.Part1
import Day17.Part1

shortestPathToBottomRight' :: Grid -> Int
shortestPathToBottomRight' grid = shortestPathFromTopLeft (snd $ A.bounds grid) grid neighborCriteria'

neighborCriteria' :: Steps -> Direction -> Direction -> Bool
neighborCriteria' sameSteps stepDir newDir =
    (newDir /= opposite stepDir)
        && ((sameSteps <= 10) || (newDir /= stepDir))
        && ((newDir == stepDir) || (sameSteps >= 4))
