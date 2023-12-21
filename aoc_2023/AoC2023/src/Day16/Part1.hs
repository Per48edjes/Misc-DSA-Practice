{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Day16.Part1 where

import Control.Applicative ((<|>))
import Control.Monad.State
import Data.Array (Array)
import qualified Data.Array as A
import Data.Attoparsec.Text
import Data.Bifunctor (bimap)
import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Text.IO as TIO

import Day10.Part1 (Coord, Direction (..), Opposable (..))

-- Types

data Tile = Empty | Mirror Direction Direction Direction Direction | Splitter Direction Direction
  deriving (Eq, Show)

type TileGrid = Array Coord Tile

type LightRay = (Coord, Direction)
type LightState = State ([LightRay], S.Set LightRay) Int

-- Functions

solution :: FilePath -> (Array Coord Tile -> Int) -> IO Int
solution filePath valueFunc = do
  contents <- TIO.readFile filePath
  let tileArr = case parseOnly (evalStateT statefulParseGrid (1, 0)) contents of
        Left err -> error err
        Right arr -> arr
  return $ valueFunc tileArr

countEnergizedTiles :: TileGrid -> Int
countEnergizedTiles tileGrid = evalState (traverseGrid tileGrid) ([((0, -1), E)], S.empty)

traverseGrid :: TileGrid -> LightState
traverseGrid tileGrid = do
  (heads, visited) <- get
  if null heads
    then do
      let uniqVisitedCoords = (length . filter (A.inRange (A.bounds tileGrid)) . L.nub) [c | (c, _) <- S.toList visited]
      return uniqVisitedCoords
    else do
      let (newHeads, newVisited) = L.foldl' (\(hs, vs) h -> traverseTile tileGrid h hs vs) ([], visited) heads
      put (newHeads, newVisited)
      traverseGrid tileGrid

traverseTile :: TileGrid -> LightRay -> [LightRay] -> S.Set LightRay -> ([LightRay], S.Set LightRay)
traverseTile tileGrid ray@((x, y), dir) heads visited
  | ray `S.member` visited = (heads, visited)
  | not . A.inRange (A.bounds tileGrid) $ coord' = (heads, S.insert ray visited)
  | otherwise =
      let newDirs = getNewDirections (tileGrid A.! coord') (opposite dir)
       in (((coord',) <$> newDirs) <> heads, S.insert ray visited)
 where
  coord' = bimap (+ x) (+ y) (vectorDir dir)

-- | Takes a tile and incoming ray direction and returns the outgoing ray direction(s)
getNewDirections :: Tile -> Direction -> [Direction]
getNewDirections Empty dir = [opposite dir]
getNewDirections (Mirror d _ _ _) N = [d]
getNewDirections (Mirror _ d _ _) S = [d]
getNewDirections (Mirror _ _ d _) E = [d]
getNewDirections (Mirror _ _ _ d) W = [d]
getNewDirections (Splitter E W) N = [E, W]
getNewDirections (Splitter E W) S = [E, W]
getNewDirections (Splitter E W) dir = [opposite dir]
getNewDirections (Splitter N S) E = [N, S]
getNewDirections (Splitter N S) W = [N, S]
getNewDirections (Splitter N S) dir = [opposite dir]
getNewDirections _ _ = error "Invalid tile type"

vectorDir :: Direction -> Coord
vectorDir N = (-1, 0)
vectorDir S = (1, 0)
vectorDir E = (0, 1)
vectorDir W = (0, -1)
vectorDir _ = error "Invalid light direction"

-- Parsing

statefulParseGrid :: StateT Coord Parser (Array Coord Tile)
statefulParseGrid = do
  initCoords <- get
  (tiles, (x, y)) <- lift $ runStateT (manyTill' statefulParseTile (lift endOfInput)) initCoords
  return $ A.array ((0, 0), (x - 1, y - 1)) $ zip (A.range ((0, 0), (x - 1, y - 1))) tiles

statefulParseTile :: StateT Coord Parser Tile
statefulParseTile = do
  (x, y) <- get
  c <- lift parseTile
  case c of
    Just t -> put (x, y + 1) >> return t
    Nothing -> put (x + 1, 0) >> statefulParseTile
 where
  parseTile = (Just . toTile <$> notChar '\n') <|> (endOfLine >> return Nothing)

toTile :: Char -> Tile
toTile '\\' = Mirror E W N S
toTile '/' = Mirror W E S N
toTile '|' = Splitter N S
toTile '-' = Splitter E W
toTile '.' = Empty
toTile e = error $ "Invalid tile type: " ++ show [e]
