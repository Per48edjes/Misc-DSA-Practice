{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Day18.Part1 (solution, totalTrenchVolume) where

import Control.Monad.State.Strict
import Data.Attoparsec.Text
import Data.Bifunctor (bimap)
import Data.Functor (($>))
import Data.Ix (Ix (inRange))
import qualified Data.List as L
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

type FillState = State (Set Coord) ()

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
calculateTrenchInterior trench = S.size (getInteriorCoords trench)

getTrench :: [PlanEntry] -> Set (Index, Coord)
getTrench plan =
    let (_, visited) = execState (mapM_ dig idxPlan) ((0, 0), S.empty)
     in visited
  where
    idxPlan = zip [0 ..] plan

getTrenchCoords :: Set (Index, Coord) -> Set Coord
getTrenchCoords = S.map snd

-- | Uses flood fill of exterior to impute interior coordinate set
getInteriorCoords :: Set (Index, Coord) -> Set Coord
getInteriorCoords trench =
    let ((minX, maxX), (minY, maxY)) = getBoundingDimensions trench
        ((minX', maxX'), (minY', maxY')) = ((minX - 1, maxX + 1), (minY - 1, maxY + 1))
        candidateCoords = S.fromList [(x, y) | x <- [minX' .. maxX'], y <- [minY' .. maxY']]
     in candidateCoords S.\\ getExteriorCoords trench

dig :: (Index, PlanEntry) -> DigState
dig (i, (dir, steps, _)) = do
    (curCoord, visited) <- get
    let (x', y') = vectorDir dir
        newCoords = fmap (i,) $ tail $ L.take (steps + 1) $ iterate (\(x, y) -> (x + x', y + y')) curCoord
        newVisited = S.union visited (S.fromList newCoords)
    put (snd . last $ newCoords, newVisited)

getExteriorCoords :: Set (Index, Coord) -> Set Coord
getExteriorCoords trench = execState (floodFillTrench trench' augmentedBounds (minX', minY')) S.empty
  where
    ((minX, maxX), (minY, maxY)) = getBoundingDimensions trench
    augmentedBounds@((minX', _), (minY', _)) = ((minX - 1, maxX + 1), (minY - 1, maxY + 1))
    trench' = getTrenchCoords trench

floodFillTrench :: Set Coord -> ((Index, Index), (Index, Index)) -> Coord -> FillState
floodFillTrench trench' augmentedBounds@((minX', maxX'), (minY', maxY')) curCoord = do
    visited <- get
    unless (curCoord `S.member` visited) $ do
        let neighbors = getNeighbors curCoord
            unvisitedNeighbors = filter (`S.notMember` visited) neighbors
        put (S.insert curCoord visited)
        mapM_ (floodFillTrench trench' augmentedBounds) unvisitedNeighbors
  where
    getNeighbors (x, y) =
        let allNeighbors = [bimap (+ x) (+ y) $ vectorDir dir | dir <- [N, S, E, W]]
         in filter (\c -> c `S.notMember` trench' && inRange ((minX', minY'), (maxX', maxY')) c) allNeighbors

-- | Gets a the bounding coordinates a trench
getBoundingDimensions :: Set (Index, Coord) -> ((Int, Int), (Int, Int))
getBoundingDimensions trench =
    let xs = S.map fst $ getTrenchCoords trench
        ys = S.map snd $ getTrenchCoords trench
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
