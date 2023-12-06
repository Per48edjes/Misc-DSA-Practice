{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Day05.Part1 (solution, minLocation, Interval, Location, Seed, Mapping, mappingToMappingFunc, seedToLocationGen) where

import Data.Attoparsec.Text
import qualified Data.Text as T
import qualified Data.Text.IO as TR

type Interval = (Int, Int, Int)
type Mapping = [Interval]
type Seed = Int
type Location = Int

solution :: FilePath -> ([Mapping] -> [Seed] -> Location) -> IO Int
solution filePath valueFunc = do
    seedLine : rest <- T.lines <$> TR.readFile filePath
    let seeds = case parseOnly parseSeeds seedLine of
            Left _ -> error "Failed to parse seeds"
            Right s -> s
        mappings = case parseOnly parseMappings (T.unlines rest) of
            Left _ -> error "Failed to parse mappings"
            Right m -> m
    return $ valueFunc mappings seeds

mappingToMappingFunc :: Mapping -> (Int -> Int)
mappingToMappingFunc mapping = mappingFunc
  where
    inRange src (_, srcStart, rangeLen) = srcStart <= src && src < srcStart + rangeLen
    mappedValue src (destStart, srcStart, _) = destStart + (src - srcStart)
    mappingFunc src = case applyFilter (inRange src) (mappedValue src) mapping of
        [] -> src
        (x : _) -> x

applyFilter :: (a -> Bool) -> (a -> b) -> [a] -> [b]
applyFilter predicate transform list = map transform $ filter predicate list

seedToLocationGen :: [Mapping] -> (Mapping -> (a -> a)) -> (a -> a)
seedToLocationGen mappings f = foldl (.) id (reverse $ f <$> mappings)

minLocation :: [Mapping] -> [Seed] -> Location
minLocation mappings = minimum . map (seedToLocationGen mappings mappingToMappingFunc)

-- Parsers

parseSeeds :: Parser [Seed]
parseSeeds = string "seeds: " *> decimal `sepBy` many1 space

parseInterval :: Parser Interval
parseInterval = do
    x <- decimal
    space
    y <- decimal
    space
    z <- decimal
    endOfLine
    return (x, y, z)

parseMapping :: Parser [Interval]
parseMapping = do
    endOfLine
    skipWhile (/= '\n') *> endOfLine
    many' parseInterval

parseMappings :: Parser [Mapping]
parseMappings = manyTill' parseMapping endOfInput
