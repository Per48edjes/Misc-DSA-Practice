{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Day18.Part1 (solution, totalTrenchVolume) where

import Debug.Trace (trace)

import Control.Monad.State.Strict
import Data.Attoparsec.Text
import Data.Function (on)
import Data.Functor (($>))
import qualified Data.List as L
import qualified Data.List.GroupBy as G
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Day10.Part1 (Coord, Direction (..))
import Day11.Part1 (Index)
import Day16.Part1 (vectorDir)

-- Types

type PlanEntry = (Direction, Int, Text)
type DigState = State (Coord, Set (Index, Coord)) ()

-- Functions

solution :: FilePath -> ([PlanEntry] -> Int) -> IO Int
solution filePath valueFunc = do
    input <- TIO.readFile filePath
    let digPlan = case parseOnly parsePlan input of
            Left err -> error err
            Right plan -> plan
    return $ valueFunc digPlan

totalTrenchVolume :: [PlanEntry] -> Int
totalTrenchVolume = calculateTrenchInterior . getTrench

calculateTrenchInterior :: Set (Index, Coord) -> Int
calculateTrenchInterior trench = S.size (getTrenchCoords trench) + S.size (getInteriorCoords trench)

getTrench :: [PlanEntry] -> Set (Index, Coord)
getTrench plan =
    let (_, visited) = execState (mapM_ dig idxPlan) ((0, 0), S.empty)
     in visited
  where
    idxPlan = zip [0 ..] plan

getTrenchCoords :: Set (Index, Coord) -> Set Coord
getTrenchCoords = S.map snd

getInteriorCoords :: Set (Index, Coord) -> Set Coord
getInteriorCoords trench =
    let ((minX, maxX), (minY, maxY)) = getBoundingDimensions trench
        candidateCoords = [(x, y) | x <- [minX .. maxX], y <- [minY .. maxY], (x, y) `S.notMember` S.map snd trench]
     in S.fromList $ filter (\c -> and $ (raycastDir trench <$> [S]) <*> [c]) candidateCoords

dig :: (Index, PlanEntry) -> DigState
dig (i, (dir, steps, _)) = do
    (curCoord, visited) <- get
    let (x', y') = vectorDir dir
        newCoords = fmap (i,) $ tail $ L.take (steps + 1) $ iterate (\(x, y) -> (x + x', y + y')) curCoord
        newVisited = S.union visited (S.fromList newCoords)
    put (snd . last $ newCoords, newVisited)

{-
-- BUG: Need to reevaluate grouping strategy as it doesn't work for all cases
-- WARN: Also need to consider edge case where trench loops back on itself,
-- the group logic will fail in that case if ray is othrogonal to the first
-- ans last instruction directions
-}
raycastDir :: Set (Index, Coord) -> Direction -> Coord -> Bool
raycastDir trench dir coord = odd $ length (trace (show $ "Coord: " ++ show coord ++ ", " ++ show dir ++ " Walls: " ++ show groupedTrenchCoords) groupedTrenchCoords)
  where
    ((minX, maxX), (minY, maxY)) = getBoundingDimensions trench
    (x', y') = vectorDir dir
    compVal = case dir of
        N -> minX
        S -> maxX
        E -> maxY
        W -> minY
        _ -> error "Invalid traversal direction"
    compElem = case dir of
        N -> fst
        S -> fst
        E -> snd
        W -> snd
        _ -> error "Invalid traversal direction"
    ray = S.fromList $ L.take (abs (compVal - compElem coord) + 1) $ iterate (\(x, y) -> (x + x', y + y')) coord
    trenchCoords = S.toList $ S.filter (\(_, c) -> c `S.member` ray) trench
    groupedTrenchCoords = groupTrenchWalls dir trenchCoords

groupTrenchWalls :: Direction -> [(Index, Coord)] -> [[(Index, Coord)]]
groupTrenchWalls dir trenchCoords
    | dir == E || dir == W = G.groupBy (\(i1, (_, y1)) (i2, (_, y2)) -> abs (y1 - y2) == 1 && (i1 == i2 || abs (i1 - i2) == 1)) . L.sortBy (compare `on` snd . snd) $ trenchCoords
    | dir == N || dir == S = G.groupBy (\(i1, (x1, _)) (i2, (x2, _)) -> abs (x1 - x2) == 1 && (i1 == i2 || abs (i1 - i2) == 1)) . L.sortBy (compare `on` fst . snd) $ trenchCoords
    | otherwise = error "Invalid traversal direction"

-- | Gets a the bounding coordinates a trench
getBoundingDimensions :: Set (Index, Coord) -> ((Int, Int), (Int, Int))
getBoundingDimensions coords =
    let xs = S.map fst $ S.map snd coords
        ys = S.map snd $ S.map snd coords
     in ((S.findMin xs, S.findMax xs), (S.findMin ys, S.findMax ys))

-- Parsers

parsePlan :: Parser [PlanEntry]
parsePlan = parsePlanEntry `sepBy` endOfLine

parsePlanEntry :: Parser PlanEntry
parsePlanEntry = do
    dir <- parseDirection
    space
    steps <- decimal
    space
    char '('
    color <- char '#' *> takeTill (== ')')
    char ')'
    return (dir, steps, color)

parseDirection :: Parser Direction
parseDirection =
    choice
        [ char 'R' $> E
        , char 'D' $> S
        , char 'L' $> W
        , char 'U' $> N
        ]
