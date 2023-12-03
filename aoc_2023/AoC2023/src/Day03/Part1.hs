{-# LANGUAGE OverloadedRecordDot #-}

module Day03.Part1 (solution, parseLine, Entity (..), sumPartNums, partitionEntities, getSurroundingCoords) where

import Control.Applicative
import Control.Monad.State
import Data.Attoparsec.Text
import Data.Char
import Data.Either
import Data.List (partition)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as TR

data Entity
    = PartNum
        { entityCoords :: S.Set Coord
        , entityValue :: Int
        }
    | Symbol
        { entityCoords :: S.Set Coord
        , entityText :: Text
        }
    deriving (Show, Eq)

isPartNum :: Entity -> Bool
isPartNum PartNum{} = True
isPartNum _ = False

type Row = Int
type Col = Int
type Coord = (Row, Col)

type LineParser = StateT Col Parser

solution :: FilePath -> ([[Entity]] -> Int) -> IO Int
solution filePath valueParser = do
    input <- readFile filePath
    let parsed = rights $ zipWith parseLine [0 ..] (T.lines $ T.pack input)
    return $ valueParser parsed

sumPartNums :: [[Entity]] -> Int
sumPartNums entities = sum . map entityValue . filter isNearSymbol $ partNums
  where
    (partNums, symbols) = partitionEntities entities
    symbolCoords = foldr (S.union . entityCoords) S.empty symbols
    isNearSymbol partNum = (not . S.null) $ S.intersection symbolCoords $ getSurroundingCoords partNum

partitionEntities :: [[Entity]] -> ([Entity], [Entity])
partitionEntities entities = partition isPartNum (concat entities)

getSurroundingCoords :: Entity -> S.Set Coord
getSurroundingCoords e = S.foldr (S.union . getCoordNeighbors) S.empty e.entityCoords S.\\ e.entityCoords
  where
    getCoordNeighbors (r, c) = S.fromList [(r + dr, c + dc) | dr <- [-1 .. 1], dc <- [-1 .. 1], dr /= 0 || dc /= 0]

-- | Parses a single line of the input file, giving back a list of Entities
parseLine :: Row -> Text -> Either String [Entity]
parseLine row = parseOnly (evalStateT (rowParserGenerator row) 0)

-- | Generates a stateful parser for a Row consisting of multiple Entities
rowParserGenerator :: Row -> LineParser [Entity]
rowParserGenerator row = do
    many' (entityParserGenerator row)

-- | Generates a stateful parser for a single Entity separated by dot ('.')  characters
entityParserGenerator :: Row -> LineParser Entity
entityParserGenerator row = do
    skipDots
    startCol <- get
    tokens <- lift (takeWhile1 isDigit <|> (T.singleton <$> satisfy (/= '.')))
    let lenOfSymbols = T.length tokens
        coords = S.fromList [(row, col) | col <- [startCol .. startCol + lenOfSymbols - 1]]
    put (startCol + lenOfSymbols)
    skipDots
    return $ case TR.decimal tokens of
        Right (n, _) -> PartNum coords n
        Left _ -> Symbol coords tokens
  where
    skipDots :: LineParser ()
    skipDots = do
        dotsCount <- lift $ T.length . T.pack <$> many' (char '.')
        modify (+ dotsCount)
