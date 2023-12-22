module Day17.Part1 where

import Control.Applicative ((<|>))
import Control.Monad.State
import Data.Array (Array, (!))
import qualified Data.Array as A
import Data.Attoparsec.Text
import Data.Bifunctor (bimap)
import Data.Char (digitToInt)
import Data.Heap (MinHeap)
import qualified Data.Heap as H
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Text.IO as TIO
import Day10.Part1 (Coord, Direction (..), getRelativeDirection, opposite)
import Day16.Part1 (vectorDir)

-- Types

type Weight = Int
type Distance = Int
type Steps = Int
type Grid = Array Coord Node
type PriorityQueue = MinHeap (Distance, Node, Steps, Direction)
type DistancesMap = Map Coord Int
type DijkstraState = State (PriorityQueue, DistancesMap) ()

data Node = Node {coord :: Coord, weight :: Weight}
    deriving (Eq, Show)

instance Ord Node where
    compare (Node _ w1) (Node _ w2) = compare w1 w2

-- Functions

solution :: FilePath -> (Grid -> Int) -> IO Int
solution filePath valueFunc = do
    contents <- TIO.readFile filePath
    let grid = case parseOnly (evalStateT statefulParseGrid (1, 0)) contents of
            Left err -> error err
            Right g -> g
    return $ valueFunc grid

shortestPathToBottomRight :: Grid -> Int
shortestPathToBottomRight grid = shortestPathFromTopLeft (snd $ A.bounds grid) grid

shortestPathFromTopLeft :: Coord -> Grid -> Int
shortestPathFromTopLeft endCoord grid =
    let (_, dists) = execState (modifiedDijkstra grid) (H.singleton (0, grid ! startCoord, 0, E), M.empty)
     in dists M.! endCoord - weight (grid ! startCoord)
  where
    startCoord = (0, 0)

-- BUG: This is hanging indefinitely on the example input
modifiedDijkstra :: Grid -> DijkstraState
modifiedDijkstra grid = do
    (pq, dists) <- get
    case H.view pq of
        Nothing -> return ()
        Just ((dist, node, steps, dir), pq')
            | M.member (coord node) dists && dist > (dists M.! coord node) -> do
                put (pq', dists)
                modifiedDijkstra grid
            | otherwise -> do
                let neighborTraversals = traverseNeighbors grid node steps dir
                    (pq'', dists') = foldr (relaxEdge dist) (pq', dists) neighborTraversals
                put (pq'', dists')
                modifiedDijkstra grid
  where
    relaxEdge :: Distance -> (Node, Steps, Direction) -> (PriorityQueue, DistancesMap) -> (PriorityQueue, DistancesMap)
    relaxEdge dist (node, steps, dir) (pq, dists)
        | M.member (coord node) dists && dist' < (dists M.! coord node) = (H.insert (dist', node, steps, dir) pq, M.insert (coord node) dist' dists)
        | otherwise = (pq, dists)
      where
        dist' = dist + weight node

traverseNeighbors :: Grid -> Node -> Steps -> Direction -> [(Node, Steps, Direction)]
traverseNeighbors grid (Node (x, y) _) sameSteps stepDir = do
    neighborCoord <- neighborCoords
    let neighborNode = grid ! neighborCoord
        sameSteps' = if stepDir == getRelativeDirection (x, y) neighborCoord then sameSteps + 1 else 1
    return (neighborNode, sameSteps', getRelativeDirection (x, y) neighborCoord)
  where
    neighborCoords = filter (A.inRange $ A.bounds grid) $ bimap (+ x) (+ y) . vectorDir <$> filter neighborCriteria [N, S, E, W]
    neighborCriteria dir = (dir /= opposite stepDir) && ((sameSteps < 3) || (dir /= stepDir))

-- Parsing

statefulParseGrid :: StateT Coord Parser Grid
statefulParseGrid = do
    initCoords <- get
    (nodes, (x, y)) <- lift $ runStateT (manyTill' statefulParseNode (lift endOfInput)) initCoords
    return $ A.array ((0, 0), (x - 1, y - 1)) $ zip (A.range ((0, 0), (x - 1, y - 1))) nodes

statefulParseNode :: StateT Coord Parser Node
statefulParseNode = do
    (x, y) <- get
    c <- lift parseNode
    case c of
        Just w -> put (x, y + 1) >> return (Node (x - 1, y) w)
        Nothing -> put (x + 1, 0) >> statefulParseNode
  where
    parseNode = Just . digitToInt <$> digit <|> (endOfLine >> return Nothing)
