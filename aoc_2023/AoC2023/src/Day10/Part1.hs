{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Day10.Part1 (solution, findLoopMidpointDistance) where

import Control.Applicative (liftA2)
import Control.Monad.State (State, evalState, get, put)
import Data.Attoparsec.Text
import Data.Bifunctor (bimap)
import Data.List (tails)
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text.IO as TIO
import qualified Data.Vector as V

-- Types

data Direction = N | S | E | W
    deriving (Eq, Show)

data Pipe = Pipe Direction Direction | Ground
    deriving (Eq, Show)

data Cell = CPipe Pipe | CStart
    deriving (Eq, Show)

type PipeGrid = V.Vector (V.Vector Cell)
type Coord = (Int, Int)

type DFSState = (Set Coord, Int)

-- Typeclasses & Instances

class Opposable a where
    opposite :: a -> a

class Connectable a where
    canConnect :: Direction -> a -> a -> Bool

instance Opposable Direction where
    opposite :: Direction -> Direction
    opposite N = S
    opposite S = N
    opposite E = W
    opposite W = E

instance Connectable Pipe where
    canConnect :: Direction -> Pipe -> Pipe -> Bool
    canConnect _ Ground _ = False
    canConnect _ _ Ground = False
    -- \| NOTE: Relative direction is second pipe vs. first pipe
    canConnect relDir (Pipe p1 p2) (Pipe p'1 p'2)
        | relDir == p1 && (opposite relDir == p'1 || opposite relDir == p'2) = True
        | relDir == p2 && (opposite relDir == p'1 || opposite relDir == p'2) = True
        | otherwise = False

-- Functions

solution :: FilePath -> (PipeGrid -> Int) -> IO Int
solution filePath valueFunc = do
    contents <- TIO.readFile filePath
    let pipeGrid = case parseOnly parsePipeGrid contents of
            Left err -> error err
            Right pg -> pg
    return $ valueFunc pipeGrid

findLoopMidpointDistance :: PipeGrid -> Int
findLoopMidpointDistance pipeGrid = case evalState (traverseMainLoop pipeGrid' startCoord startCoord) (S.empty, 0) of
    Just distance -> distance `div` 2
    Nothing -> error "Could not find loop midpoint"
  where
    (pipeGrid', startCoord) = updatePipeGrid pipeGrid

-- | Traverse the pipe grid from start to end, returning the distance from start to end
traverseMainLoop :: PipeGrid -> Coord -> Coord -> State DFSState (Maybe Int)
traverseMainLoop pipeGrid endCoord startCoord = do
    (visited, distance) <- get
    if startCoord == endCoord && distance > 0
        then return $ Just distance
        else do
            let neighbors = getNeighbors pipeGrid startCoord
            let unvisitedNeighbors = filter (isValidNeighbor visited) neighbors
            case unvisitedNeighbors of
                [] -> return Nothing
                -- Choose one neighbor to traverse
                ((_, nextCoord) : _) -> do
                    put (S.insert nextCoord visited, distance + 1)
                    traverseMainLoop pipeGrid endCoord nextCoord
  where
    isValidNeighbor visited (neighborPipe, neighborCoord) = neighborCoord `S.notMember` visited && canConnect (getRelativeDirection startCoord neighborCoord) (fromCoord pipeGrid startCoord) neighborPipe

-- | Update the pipe grid by replacing Start with the appropriate Pipe
updatePipeGrid :: PipeGrid -> (PipeGrid, Coord)
updatePipeGrid pG = (pG V.// [(x, pG V.! x V.// [(y, CPipe newStartPipe)])], (x, y))
  where
    startCoord = case findIndices pG CStart of
        [coord] -> coord
        _ -> error "Could not locate start"
    startPairedNeighbors = getPairedNeighbors pG startCoord
    potentialStarts = [(p, startCoord) | p <- [Pipe N E, Pipe N W, Pipe S E, Pipe S W, Pipe E W, Pipe N S]]
    (newStartPipe, x, y) = case filter (uncurry isValidPipe) (liftA2 (,) potentialStarts startPairedNeighbors) of
        (((newStartPipe, (x, y)), _) : _) -> (newStartPipe, x, y)
        _ -> error "Could not determine replacement for Start pipe"

-- | Determine whether a pipe connects between two other pipes
isValidPipe :: (Pipe, Coord) -> ((Pipe, Coord), (Pipe, Coord)) -> Bool
isValidPipe (p, coord) ((p'1, coord'1), (p'2, coord'2)) = canConnect (getRelativeDirection coord coord'1) p p'1 && canConnect (getRelativeDirection coord coord'2) p p'2

-- | Gets the neighbors of a given coordinate in a grid (if they exist)
getNeighbors :: PipeGrid -> Coord -> [(Pipe, Coord)]
getNeighbors pG (x, y) = mapMaybe (\(x', y') -> (pG V.!? x') >>= (V.!? y') >>= (\cell -> Just (fromCell cell, (x', y')))) neighborCoords
  where
    neighborCoords = map (bimap (x +) (y +)) [(0, 1), (0, -1), (1, 0), (-1, 0)]

-- | Gets the pairwise combinations of neighbors of a given coordinate in a grid (if they exist)
getPairedNeighbors :: PipeGrid -> Coord -> [((Pipe, Coord), (Pipe, Coord))]
getPairedNeighbors pG coord = pairwiseCombinations $ getNeighbors pG coord

-- | Gets the direction of the second coordinate relative to the first
getRelativeDirection :: Coord -> Coord -> Direction
getRelativeDirection (x, y) (x', y') = case bimap (x' -) (y' -) (x, y) of
    (-1, 0) -> N
    (1, 0) -> S
    (0, -1) -> W
    (0, 1) -> E
    _ -> error "Invalid relative direction"

findIndices :: (Eq a) => V.Vector (V.Vector a) -> a -> [Coord]
findIndices grid element =
    [(x, y) | x <- [0 .. (V.length grid - 1)], y <- [0 .. (V.length (grid V.! x) - 1)], grid V.! x V.! y == element]

pairwiseCombinations :: [a] -> [(a, a)]
pairwiseCombinations xs = [(x, y) | (x : ys) <- tails xs, y <- ys]

-- | Get the pipe from a coord
fromCoord :: PipeGrid -> Coord -> Pipe
fromCoord pG (x, y) = fromCell $ pG V.! x V.! y

-- | Get the pipe from a cell
fromCell :: Cell -> Pipe
fromCell (CPipe p) = p
fromCell _ = error "Invalid cell"

-- Parsers

parsePipeGrid :: Parser PipeGrid
parsePipeGrid = V.fromList <$> manyTill parseRow endOfInput

parseRow :: Parser (V.Vector Cell)
parseRow = V.fromList <$> manyTill parsePipe (choice [endOfLine, endOfInput])

parsePipe :: Parser Cell
parsePipe = toPipe <$> choice [parseSW, parseSE, parseNE, parseNW, parseNS, parseEW, parseEmpty, parseStart]
  where
    parseSW = char '7'
    parseSE = char 'F'
    parseNE = char 'L'
    parseNW = char 'J'
    parseNS = char '|'
    parseEW = char '-'
    parseEmpty = char '.'
    parseStart = char 'S'

toPipe :: Char -> Cell
toPipe '7' = CPipe $ Pipe S W
toPipe 'F' = CPipe $ Pipe S E
toPipe 'L' = CPipe $ Pipe N E
toPipe 'J' = CPipe $ Pipe N W
toPipe '|' = CPipe $ Pipe N S
toPipe '-' = CPipe $ Pipe E W
toPipe '.' = CPipe Ground
toPipe 'S' = CStart
toPipe _ = error "Invalid pipe type"
