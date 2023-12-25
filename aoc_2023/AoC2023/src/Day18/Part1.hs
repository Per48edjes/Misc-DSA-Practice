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
import Day16.Part1 (vectorDir)

-- Types

type PlanEntry = (Direction, Int, Text)
type DigState = State (Coord, Set Coord) ()

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

calculateTrenchInterior :: Set Coord -> Int
calculateTrenchInterior trench =
    let ((minX, maxX), (minY, maxY)) = getBoundingDimensions trench
        trenchCoords = S.toList trench
        candidateCoords = [(x, y) | x <- [minX .. maxX], y <- [minY .. maxY], (x, y) `S.notMember` trench]
     in S.size . S.fromList $ trenchCoords ++ filter (raycastLeft trench) candidateCoords

getTrench :: [PlanEntry] -> Set Coord
getTrench plan =
    let (_, visited) = execState (mapM_ dig plan) ((0, 0), S.empty)
     in visited

-- | Digs the trench according to the plan
dig :: PlanEntry -> DigState
dig (dir, steps, _) = do
    (curCoord, visited) <- get
    let (x', y') = vectorDir dir
        newCoords = L.take (steps + 1) $ iterate (\(x, y) -> (x + x', y + y')) curCoord
        newVisited = S.union visited (S.fromList newCoords)
    put (last newCoords, newVisited)

-- | Casts a ray left of the input Coord and returns True if the number of intersections with the trench is odd
raycastLeft :: Set Coord -> Coord -> Bool
raycastLeft trench coord = odd $ length groupedTrenchCoords
  where
    ((_, _), (minY, _)) = getBoundingDimensions trench
    (x', y') = vectorDir W
    trenchCoords = filter (`S.member` trench) $ L.take (abs (minY - snd coord) + 1) $ iterate (\(x, y) -> (x + x', y + y')) coord
    groupedTrenchCoords = groupByConsecutiveY trenchCoords

-- | Function to sort Coords and group by consecutive y-coordinates
groupByConsecutiveY :: [(Int, Int)] -> [[(Int, Int)]]
groupByConsecutiveY = G.groupBy (\a b -> succ (snd a) == snd b) . L.sortBy (compare `on` snd)

-- | Gets a the bounding coordinates a trench
getBoundingDimensions :: Set Coord -> ((Int, Int), (Int, Int))
getBoundingDimensions coords =
    let xs = S.map fst coords
        ys = S.map snd coords
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
