{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Day14.Part1 where

import Control.Applicative ((<|>))
import Control.Monad.State
import Data.Array (Array)
import qualified Data.Array as A
import Data.Attoparsec.Text
import qualified Data.List as L
import Data.List.Split (chunksOf)
import qualified Data.Text.IO as TIO

-- Types

type Coord = (Int, Int)
data Cell = Empty | Rounded | Cubed deriving (Show, Enum, Ord, Eq)

-- Functions

solution :: FilePath -> (Array Coord Cell -> Int) -> IO Int
solution filePath valueFunc = do
    contents <- TIO.readFile filePath
    let rockArr = case parseOnly (evalStateT statefulParsePlatform (1, 0)) contents of
            Left err -> error err
            Right arr -> arr
    return $ valueFunc rockArr

calculateLoad :: Array Coord Cell -> Int
calculateLoad = calculateRightLoad . tiltRight . rotateRight

calculateRightLoad :: Array Coord Cell -> Int
calculateRightLoad arr = sum $ (\((_, y), cell) -> if cell == Rounded then y + 1 else 0) <$> A.assocs arr

-- | Tilt platform to the right
tiltRight :: Array Coord Cell -> Array Coord Cell
tiltRight arr = A.array bounds indexedElements
  where
    bounds@((_, _), (_, maxY)) = A.bounds arr
    transformRow = concatMap L.sort . L.groupBy (\a b -> a /= Cubed && b /= Cubed)
    rows = chunksOf (1 + maxY) (A.elems arr)
    rows' = transformRow <$> rows
    indexedElements = [((i, j), x) | (i, row) <- zip [0 ..] rows', (j, x) <- zip [0 ..] row]

-- | Rotate the platform clockwise
rotateRight :: Array Coord Cell -> Array Coord Cell
rotateRight arr = rotateDir (clockwise arr) arr

-- | Rotate the platform counterclockwise
rotateLeft :: Array Coord Cell -> Array Coord Cell
rotateLeft arr = rotateDir (counterClockwise arr) arr

-- | Rotate the platform in a direction given by `dirFunc`
rotateDir :: (Coord -> Coord) -> Array Coord Cell -> Array Coord Cell
rotateDir dirFunc arr = A.ixmap (A.bounds arr) dirFunc arr

-- | Coordinate transform to rotate the platform clockwise; assumes square platform
clockwise :: Array Coord Cell -> Coord -> Coord
clockwise arr (x, y) = (offset - y, x)
  where
    offset = fst $ snd $ A.bounds arr

-- | Coordinate transform to rotate the platform counterclockwise; assumes square platform
counterClockwise :: Array Coord Cell -> Coord -> Coord
counterClockwise arr (x, y) = (y, offset - x)
  where
    offset = snd $ snd $ A.bounds arr

-- Parsing

statefulParsePlatform :: StateT Coord Parser (Array Coord Cell)
statefulParsePlatform = do
    initCoords <- get
    (cells, (x, y)) <- lift $ runStateT (manyTill' statefulParseCell (lift endOfInput)) initCoords
    return $ A.array ((0, 0), (x - 1, y - 1)) $ zip (A.range ((0, 0), (x - 1, y - 1))) cells

statefulParseCell :: StateT Coord Parser Cell
statefulParseCell = do
    (x, y) <- get
    c <- lift parseCell
    case c of
        Just '.' -> put (x, y + 1) >> return Empty
        Just 'O' -> put (x, y + 1) >> return Rounded
        Just '#' -> put (x, y + 1) >> return Cubed
        Nothing -> put (x + 1, 0) >> statefulParseCell
        _ -> error "Invalid cell"
  where
    parseCell = Just <$> (char '.' <|> char 'O' <|> char '#') <|> (endOfLine >> return Nothing)
