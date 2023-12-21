module Day17.Part1 where

import Control.Applicative ((<|>))
import Control.Monad.State.Strict
import Data.Array (Array, (!))
import qualified Data.Array as A
import Data.Attoparsec.Text
import Data.Bifunctor (bimap)
import Data.Char (digitToInt)
import Data.Heap (MinPrioHeap)
import qualified Data.Heap as H
import qualified Data.List as L
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Text.IO as TIO
import Day10.Part1 (Coord, Direction (..), isOrthogonal, opposite)
import Day16.Part1 (vectorDir)
import Util

-- Types & Instances

type Weight = Int
type Steps = Int
type Grid = Array Coord Node
type NodeState = (Node, Steps, Direction)
type PriorityQueue = MinPrioHeap Distance NodeState
type DistancesMap = Map NodeState Distance
type DijkstraState = State (PriorityQueue, DistancesMap) ()

data Distance = Finite Int | Infinity
    deriving (Eq, Ord, Show)

data Node = Node {coord :: Coord, weight :: Weight}
    deriving (Eq, Ord, Show)

instance Bounded Distance where
    minBound = Finite 0
    maxBound = Infinity

instance Num Distance where
    Infinity + _ = Infinity
    _ + Infinity = Infinity
    Finite x + Finite y = Finite (x + y)

    Infinity * _ = Infinity
    _ * Infinity = Infinity
    Finite x * Finite y = Finite (x * y)

    abs Infinity = Infinity
    abs (Finite x) = Finite (abs x)

    signum Infinity = Infinity
    signum (Finite x) = Finite (signum x)

    fromInteger = Finite . fromInteger

    negate = undefined

-- Functions

solution :: FilePath -> (Grid -> Int) -> IO Int
solution filePath valueFunc = do
    contents <- TIO.readFile filePath
    let grid = case parseOnly (evalStateT statefulParseGrid (1, 0)) contents of
            Left err -> error err
            Right g -> g
    return $ valueFunc grid

shortestPathFromTopLeft :: Grid -> Int
shortestPathFromTopLeft grid =
    let
        srcs = [(Finite 0, (grid ! (0, 0), 0, dir)) | dir <- [N, S, E, W]]
        initialState = (H.fromList srcs :: PriorityQueue, M.fromList (swap <$> srcs))
        endCoord = snd (A.bounds grid)
        (_, dists) = execState (modifiedDijkstra grid endCoord getNeighborNodesState) initialState
        ds = M.filterWithKey (\(Node coord _, _, _) _ -> coord == endCoord) dists
        bottomRightSSSP = if M.null ds then Infinity else minimum ds
     in
        case bottomRightSSSP of
            Infinity -> error "No path found"
            Finite d -> d

modifiedDijkstra :: Grid -> Coord -> (Grid -> NodeState -> [NodeState]) -> DijkstraState
modifiedDijkstra grid endCoord neighborFinder = do
    (pq, dists) <- get
    case H.view pq of
        Nothing -> return ()
        Just ((_, srcNodeState), pq') -> do
            let
                neighbors = neighborFinder grid srcNodeState
                (pq'', dists') = L.foldl' (relaxEdge srcNodeState) (pq', dists) neighbors
            put (pq'', dists')
            modifiedDijkstra grid endCoord neighborFinder
  where
    relaxEdge :: NodeState -> (PriorityQueue, DistancesMap) -> NodeState -> (PriorityQueue, DistancesMap)
    relaxEdge src (pq, dists) dst@(dstNode, _, _)
        | M.notMember dst dists || dstWeight + dists M.! src < dists M.! dst = (H.insert (dstWeight + dists M.! src, dst) pq, M.insert dst (dstWeight + dists M.! src) dists)
        | otherwise = (pq, dists)
      where
        dstWeight = Finite $ weight dstNode

getNeighborNodesState :: Grid -> NodeState -> [NodeState]
getNeighborNodesState grid (Node (x, y) _, srcSteps, srcDir) = do
    dir <- [N, S, E, W]
    let (x', y') = bimap (+ x) (+ y) (vectorDir dir)
    guard $ A.inRange (A.bounds grid) (x', y')
    dstSteps <- case srcSteps of
        0 -> return 1
        pos
            | pos < 3 -> if srcDir == dir then return (pos + 1) else guard (dir /= opposite srcDir) >> return 1
            | otherwise -> guard (isOrthogonal dir srcDir) >> return 1
    return (grid ! (x', y'), dstSteps, dir)

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
