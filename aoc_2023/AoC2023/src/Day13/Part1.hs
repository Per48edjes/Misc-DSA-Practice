module Day13.Part1 (VecPattern, Pattern, Index, solution, pairHorizontalVerticalPatterns, genMatchingIndices, summarizePatterns) where

import Control.Applicative ((<|>))
import Data.Attoparsec.Text
import qualified Data.List as L
import qualified Data.Text.IO as TIO
import qualified Data.Vector as V

-- Types

type Index = Int
type VecPattern = V.Vector (V.Vector Terrain)
type Pattern = [[Terrain]]
data Terrain = Ash | Rock deriving (Eq, Show)

-- Functions

solution :: FilePath -> ([Pattern] -> Int) -> IO Int
solution filePath valueFunc = do
    contents <- TIO.readFile filePath
    let patterns = case parseOnly parsePatterns contents of
            Left err -> error err
            Right ps -> ps
    return $ valueFunc patterns

summarizePatterns :: [Pattern] -> Int
summarizePatterns = sum . map summarizePattern

summarizePattern :: Pattern -> Int
summarizePattern pattern = sum ((+ 1) <$> vReflecting) + (100 * sum ((+ 1) <$> hReflecting))
  where
    (hPattern, vPattern) = pairHorizontalVerticalPatterns pattern
    hMatches = findMatchingConsecutiveRows pattern
    vMatches = findMatchingConsecutiveRows (L.transpose pattern)
    hReflecting = filter (isReflectingIndex hPattern) hMatches
    vReflecting = filter (isReflectingIndex vPattern) vMatches

findMatchingConsecutiveRows :: Pattern -> [Int]
findMatchingConsecutiveRows pattern = map snd $ filter fst pairedRows
  where
    pairedRows = zip (zipWith (==) pattern (tail pattern)) [0 ..]

pairHorizontalVerticalPatterns :: Pattern -> (VecPattern, VecPattern)
pairHorizontalVerticalPatterns pattern = (horizontal, vertical)
  where
    horizontal = V.fromList (map V.fromList pattern)
    vertical = V.fromList (map V.fromList (L.transpose pattern))

isReflectingIndex :: VecPattern -> Index -> Bool
isReflectingIndex pattern idx = and [pattern V.! i == pattern V.! j | (i, j) <- genMatchingIndices idx, i < length pattern, j < length pattern]

genMatchingIndices :: Index -> [(Index, Index)]
genMatchingIndices n = zip [n, n - 1 .. 0] [n + 1 ..]

-- Parsers

parsePatterns :: Parser [Pattern]
parsePatterns = many' $ parsePattern <* option () endOfLine <* (endOfInput <|> pure ())

parsePattern :: Parser Pattern
parsePattern = many1 parsePatternRow

parsePatternRow :: Parser [Terrain]
parsePatternRow = many1 parseTerrain <* endOfLine

parseTerrain :: Parser Terrain
parseTerrain = do
    c <- char '#' <|> char '.'
    case c of
        '#' -> return Ash
        '.' -> return Rock
        _ -> fail "Invalid terrain"
