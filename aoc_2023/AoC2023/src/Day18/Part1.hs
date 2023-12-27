{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Day18.Part1 (solution, totalTrenchVolume, plotCoordinates) where

import Graphics.Rendering.Chart.Backend.Diagrams
import Graphics.Rendering.Chart.Easy hiding (Index)

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

plotCoordinates :: FilePath -> IO ()
plotCoordinates filePath = do
    input <- TIO.readFile filePath
    let trench = case parseOnly parsePlan input of
            Left err -> error err
            Right plan -> getTrench plan
        interior = S.toList $ getInteriorCoords trench
        bounding = S.toList $ getTrenchCoords trench
    toFile def "plot.svg" $ do
        layout_title .= "Day 18: Trench Digging"
        setColors [opaque blue, opaque red]
        plot (points "Trench" bounding)
        plot (points "Interior Points" interior)

totalTrenchVolume :: [PlanEntry] -> Int
totalTrenchVolume plan = (S.size . getTrenchCoords . getTrench $ plan) + (calculateTrenchInterior . getTrench $ plan)

calculateTrenchInterior :: Set (Index, Coord) -> Int
calculateTrenchInterior trench = S.size (getInteriorCoords trench)

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
        candidateCoords = [(x, y) | x <- [minX .. maxX], y <- [minY .. maxY], (x, y) `S.notMember` getTrenchCoords trench]
     in S.fromList $ filter (raycastLeft trench) candidateCoords

dig :: (Index, PlanEntry) -> DigState
dig (i, (dir, steps, _)) = do
    (curCoord, visited) <- get
    let (x', y') = vectorDir dir
        newCoords = fmap (i,) $ tail $ L.take (steps + 1) $ iterate (\(x, y) -> (x + x', y + y')) curCoord
        newVisited = S.union visited (S.fromList newCoords)
    put (snd . last $ newCoords, newVisited)

-- | Point-in-a-polygon algorithm that casts ray from right (leftward) and counts the number of times it enters and leaves polygon formed by the trench walls
raycastLeft :: Set (Index, Coord) -> Coord -> Bool
raycastLeft trench coord@(x, _) =
    let
        ((_, _), (_, maxY)) = getBoundingDimensions trench
        (x', y') = vectorDir W
        ray = S.fromList $ L.takeWhile (/= coord) $ iterate (\c -> (fst c + x', snd c + y')) (x, maxY + 1)
        trenchCoords = S.toList $ S.filter (\(_, c) -> c `S.member` ray) trench
     in
        (odd . sum . determineGroupConcavity . groupTrenchWalls) trenchCoords
  where
    determineGroupConcavity :: [[(Index, Coord)]] -> [Int]
    determineGroupConcavity groups = do
        group <- groups
        let (minGroupY, maxGroupY) = (snd $ snd (head group), snd $ snd (last group))
            isConvex =
                S.member (x - 1, minGroupY) trench'
                    && S.member (x + 1, maxGroupY) trench'
                    || S.member (x + 1, minGroupY) trench'
                        && S.member (x - 1, maxGroupY) trench'
        return $ if isConvex then 1 else 0
      where
        trench' = getTrenchCoords trench
    groupTrenchWalls :: [(Index, Coord)] -> [[(Index, Coord)]]
    groupTrenchWalls = G.groupBy (\(i1, (_, y1)) (i2, (_, y2)) -> abs (y1 - y2) == 1 && (i1 == i2 || abs (i1 - i2) == 1 || abs (i1 - i2) == maxIdx)) . L.sortBy (compare `on` snd . snd)
      where
        maxIdx = maximum . S.map fst $ trench

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
