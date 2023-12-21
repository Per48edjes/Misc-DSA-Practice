module Day17.Part1 where

import Control.Applicative ((<|>))
import Control.Monad.State
import Data.Array (Array, (!))
import qualified Data.Array as A
import Data.Attoparsec.Text
import Data.Char (digitToInt)
import Data.Heap (MinHeap)
import qualified Data.Heap as H
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Text.IO as TIO
import Day10.Part1 (Coord, Direction (..))
import Day16.Part1 (vectorDir)

-- TODO: Implement a modified Dijkstra's algorithm to find the shortest path on a grid graph with positive
-- edge weights where only at MOST 3 edges in the same "direction" can be traversed in a row.

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

-- NOTE: Will need to subtract starting node weight from the total distance.
shortestPath :: Grid -> Int
shortestPath grid = undefined

modifiedDijkstra :: Grid -> DijkstraState
modifiedDijkstra grid = do
    (pq, dists) <- get
    case H.view pq of
        Nothing -> return ()
        Just ((dist, node, steps, dir), pq')
            | M.member (coord node) dists && dist > (dists M.! coord node) -> modifiedDijkstra grid
            | otherwise -> do
                let dists' = M.insert (coord node) dist dists
                    neighborTraversals = traverseNeighbors grid node steps dir
                    pq'' = foldr (\(d, n, s, d') -> H.insert (d, n, s, d')) pq' neighborTraversals
                put (pq'', dists')
                modifiedDijkstra grid

traverseNeighbors :: Grid -> Node -> Steps -> Direction -> [(Distance, Node, Steps, Direction)]
traverseNeighbors grid node sameSteps whenceDir = undefined

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
    parseNode = (Just . digitToInt <$> digit) <|> (endOfLine >> return Nothing)
