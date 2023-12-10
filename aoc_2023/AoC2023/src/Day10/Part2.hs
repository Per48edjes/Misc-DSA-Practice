{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Day10.Part2 (countLoopContainedElements) where

import Control.Monad.State
import Data.Bifunctor (bimap)
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Vector as V
import Day10.Part1

-- Types

-- | Keeps track of the elements in the main loop & the elements in the "left" partition
type DFSPartitionState = (Set Coord, Set Coord, Coord)

-- Functions

countLoopContainedElements :: PipeGrid -> Int
countLoopContainedElements pipeGrid =
    let (mainLoopPartition'', leftPartition'', _) = execState (traverseMainLoop' pipeGrid'' startCoord) (S.empty, S.empty, startCoord)
        (leftFilled'', leftFilledCount'') = execState (mapM_ (floodFill pipeGrid'') $ S.toList leftPartition'') (mainLoopPartition'', 0)
        isLeftOutside = S.member (0, 0) leftFilled''
     in if isLeftOutside then numElements'' - S.size leftFilled'' else leftFilledCount''
  where
    pipeGrid' = addGroundBorder pipeGrid
    (pipeGrid'', startCoord) = updatePipeGrid pipeGrid'
    numElements'' = V.length pipeGrid' * V.length (V.head pipeGrid')

-- | Flood fills the grid from a given coordinate
floodFill :: PipeGrid -> Coord -> State DFSState ()
floodFill pipeGrid curCoord = do
    (visited, count) <- get
    unless (curCoord `S.member` visited) $ do
        let neighbors = map snd $ getNeighbors' pipeGrid curCoord
            unvisitedNeighbors = filter (`S.notMember` visited) neighbors
        put (S.insert curCoord visited, count + 1)
        mapM_ (floodFill pipeGrid) unvisitedNeighbors

-- | Traverses main loop, collecting elements in the main loop & elements in "left" partition
traverseMainLoop' :: PipeGrid -> Coord -> State DFSPartitionState ()
traverseMainLoop' pipeGrid curCoord = do
    (visited, leftPartition, prevCoord) <- get
    let neighbors = getNeighbors pipeGrid curCoord
        unvisitedNeighbors = filter (isValidNeighbor visited) neighbors
    case unvisitedNeighbors of
        -- At last element of main loop before start
        [] -> void (handleNeighbor visited leftPartition prevCoord curCoord)
        -- Choose one neighbor to traverse
        [(_, nextCoord)] -> handleNeighbor visited leftPartition prevCoord nextCoord >> traverseMainLoop' pipeGrid nextCoord
        -- Initial case when two neighbors (and directions) to choose from
        [(_, nextCoord), (_, otherCoord)] -> handleNeighbor visited leftPartition otherCoord nextCoord >> traverseMainLoop' pipeGrid nextCoord
        _ -> error "Impossible state of neighbors"
  where
    isValidNeighbor visited (neighborPipe, neighborCoord) = neighborCoord `S.notMember` visited && canConnect (getRelativeDirection curCoord neighborCoord) (fromCoord pipeGrid curCoord) neighborPipe
    handleNeighbor visited leftPartition refCoord nextCoord = do
        let relDir = getRelativeDirection curCoord refCoord
            leftCoords = S.fromList (getLeftCoords pipeGrid curCoord relDir) S.\\ visited
            leftPartition' = if nextCoord `S.member` leftPartition then S.delete curCoord leftPartition else leftPartition
        put (S.insert curCoord visited, S.union leftPartition' leftCoords, curCoord)

-- | Adds a border of Ground elements to the grid
addGroundBorder :: PipeGrid -> PipeGrid
addGroundBorder pipeGrid =
    let groundRow = (addLRGroundBorder $ V.replicate (V.length $ V.head pipeGrid) (CPipe Ground))
     in groundRow `V.cons` (addLRGroundBorder <$> pipeGrid) `V.snoc` groundRow
  where
    addLRGroundBorder row = CPipe Ground `V.cons` row `V.snoc` CPipe Ground

-- | Identifies coordinates of potential "left" partition elements vis-a-vis a main loop element
getLeftCoords :: PipeGrid -> Coord -> Direction -> [Coord]
getLeftCoords pipeGrid coord@(x, y) predecessorRelDir = case fromCoord pipeGrid coord of
    Pipe N S
        | predecessorRelDir == N -> [offset (dirToOffset d) | d <- [E]]
        | otherwise -> [offset (dirToOffset d) | d <- [W]]
    Pipe E W
        | predecessorRelDir == E -> [offset (dirToOffset d) | d <- [S]]
        | otherwise -> [offset (dirToOffset d) | d <- [N]]
    Pipe N W
        | predecessorRelDir == W -> [offset (dirToOffset d) | d <- []]
        | otherwise -> [offset (dirToOffset d) | d <- [E, S, SE]]
    Pipe N E
        | predecessorRelDir == N -> [offset (dirToOffset d) | d <- []]
        | otherwise -> [offset (dirToOffset d) | d <- [W, S, SW]]
    Pipe S W
        | predecessorRelDir == S -> [offset (dirToOffset d) | d <- []]
        | otherwise -> [offset (dirToOffset d) | d <- [N, E, NE]]
    Pipe S E
        | predecessorRelDir == E -> [offset (dirToOffset d) | d <- []]
        | otherwise -> [offset (dirToOffset d) | d <- [N, W, NW]]
    _ -> error "Invalid pipe type"
  where
    offset = bimap (x +) (y +)

-- | Gets the neighbors of a given coordinate in a grid (if they exist)
getNeighbors' :: PipeGrid -> Coord -> [(Pipe, Coord)]
getNeighbors' pG (x, y) = mapMaybe (\(x', y') -> (pG V.!? x') >>= (V.!? y') >>= (\cell -> Just (fromCell cell, (x', y')))) neighborCoords
  where
    neighborCoords = map (bimap (x +) (y +)) $ dirToOffset <$> [N, S, E, W, NW, NE, SW, SE]

dirToOffset :: Direction -> Coord
dirToOffset N = (-1, 0)
dirToOffset S = (1, 0)
dirToOffset E = (0, 1)
dirToOffset W = (0, -1)
dirToOffset NW = (-1, -1)
dirToOffset NE = (-1, 1)
dirToOffset SW = (1, -1)
dirToOffset SE = (1, 1)
