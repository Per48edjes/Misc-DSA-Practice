module Day17.Part2 where

import Control.Monad.State.Strict
import Data.Array ((!))
import qualified Data.Array as A
import Data.Bifunctor (bimap)
import qualified Data.Heap as H
import qualified Data.Map.Strict as M
import Day10.Part1
import Day16.Part1 (vectorDir)
import Day17.Part1
import Util

shortestPathFromTopLeft' :: Grid -> Int
shortestPathFromTopLeft' grid =
    let
        srcs = [(Finite 0, (grid ! (0, 0), 0, dir)) | dir <- [N, S, E, W]]
        initialState = (H.fromList srcs :: PriorityQueue, M.fromList (swap <$> srcs))
        endCoord = snd (A.bounds grid)
        (_, dists) = execState (modifiedDijkstra grid endCoord getNeighborNodesState') initialState
        ds = M.filterWithKey (\(Node coord _, sameSteps, _) _ -> coord == endCoord && sameSteps >= 4) dists
        bottomRightSSSP = if M.null ds then Infinity else minimum ds
     in
        case bottomRightSSSP of
            Infinity -> error "No path found"
            Finite d -> d

getNeighborNodesState' :: Grid -> NodeState -> [NodeState]
getNeighborNodesState' grid (Node (x, y) _, srcSteps, srcDir) = do
    dir <- [N, S, E, W]
    let (x', y') = bimap (+ x) (+ y) (vectorDir dir)
    guard $ A.inRange (A.bounds grid) (x', y')
    dstSteps <- case srcSteps of
        0 -> return 1
        pos
            | pos < 4 -> guard (dir == srcDir) >> return (pos + 1)
            | pos < 10 -> if srcDir == dir then return (pos + 1) else guard (dir /= opposite srcDir) >> return 1
            | otherwise -> guard (isOrthogonal dir srcDir) >> return 1
    return (grid ! (x', y'), dstSteps, dir)
