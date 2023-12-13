module Day11.Part1 where

import Control.Applicative ((<|>))
import Control.Monad.State
import Data.Attoparsec.Text
import qualified Data.List as L
import qualified Data.Text.IO as TIO
import qualified Data.Vector as V
import Util (pairwiseCombinations)

-- Types

data Node a = Galaxy a | Space
    deriving (Eq, Show)
type Coord = (Index, Index)
type Index = Int
type Grid = [[Node Int]]
type ExpandedGrid = V.Vector (V.Vector (Node Int))
type AugmentedGrid = V.Vector (V.Vector (Node Int, Coord))

-- Functions

solution :: FilePath -> (Grid -> Int) -> IO Int
solution filePath valueFunc = do
    contents <- TIO.readFile filePath
    let grid = case parseOnly (evalStateT parseGrid 0) contents of
            Left err -> error err
            Right g -> g
    return $ valueFunc grid

sumGalaxyAPSPs :: Grid -> Int
sumGalaxyAPSPs grid = sum $ map (\((_, c1), (_, c2)) -> calculateSP c1 c2) galaxyPairs
  where
    galaxyPairs = pairwiseCombinations . collectGalaxies . augmentGrid . transformGrid $ grid

calculateSP :: Coord -> Coord -> Int
calculateSP (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

collectGalaxies :: AugmentedGrid -> [(Node Int, Coord)]
collectGalaxies grid = V.toList $ V.filter (\(node, _) -> node /= Space) $ V.concat $ V.toList grid

augmentGrid :: ExpandedGrid -> AugmentedGrid
augmentGrid = V.imap (\i row -> V.imap (\j node -> (node, (i, j))) row)

transformGrid :: Grid -> ExpandedGrid
transformGrid grid = V.fromList $ map V.fromList grid''
  where
    grid' = expandGridRows grid $ findEmptyRows grid
    grid'' = L.transpose $ expandGridRows (L.transpose grid') $ findEmptyRows (L.transpose grid')

expandGridRows :: Grid -> [Index] -> Grid
expandGridRows grid [] = grid
expandGridRows grid (i : rest) = expandGridRows (insertEmptyRow grid i) $ map (+ 1) rest

findEmptyRows :: Grid -> [Index]
findEmptyRows grid =
    let blankRow = replicate (length $ head grid) Space
     in L.sort $ L.elemIndices blankRow grid

insertEmptyRow :: Grid -> Index -> Grid
insertEmptyRow grid i =
    let (top, bottom) = splitAt i grid
     in top ++ [replicate (length $ head grid) Space] ++ bottom

-- Parsers

parseGrid :: StateT Int Parser [[Node Int]]
parseGrid = manyTill' parseLine (lift endOfInput)

parseLine :: StateT Int Parser [Node Int]
parseLine = do
    nodes <- many' parseNode
    lift (option () endOfLine)
    return nodes

parseNode :: StateT Int Parser (Node Int)
parseNode = do
    i <- get
    c <- lift $ char '.' <|> char '#'
    if c == '.'
        then return Space
        else do
            let i' = i + 1
            put i'
            return (Galaxy i')
