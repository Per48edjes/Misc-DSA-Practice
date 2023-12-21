module Day16.Part2 where

import Control.Monad.State
import qualified Data.Array as A
import qualified Data.Set as S
import Day10.Part1 (Direction (..))
import Day16.Part1

countEnergizedTiles' :: TileGrid -> Int
countEnergizedTiles' tileGrid = maximum $ [evalState (traverseGrid tileGrid) ([start], S.empty) | start <- getEntryPoints tileGrid]

getEntryPoints :: TileGrid -> [LightRay]
getEntryPoints tileGrid = boundaryIndices >>= genEntryPoints
  where
    ((minX, minY), (maxX, maxY)) = A.bounds tileGrid
    boundaryIndices =
        let isBoundary (x, y) = x == minX || x == maxX || y == minY || y == maxY
         in filter isBoundary $ A.indices tileGrid
    genEntryPoints (x, y)
        | x == minX && y == minY = [((x, y - 1), E), ((x - 1, y), S)]
        | x == minX && y == maxY = [((x, y + 1), W), ((x - 1, y), S)]
        | x == maxX && y == minY = [((x, y - 1), E), ((x + 1, y), N)]
        | x == maxX && y == maxY = [((x, y + 1), W), ((x + 1, y), N)]
        | x == minX = [((x - 1, y), S)]
        | x == maxX = [((x + 1, y), N)]
        | y == minY = [((x, y - 1), E)]
        | y == maxY = [((x, y + 1), W)]
        | otherwise = error "Not a boundary"
