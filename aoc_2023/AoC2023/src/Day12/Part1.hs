module Day12.Part1 where

import Control.Applicative ((<|>))
import Control.Monad.State
import Data.Attoparsec.Text (
    Parser,
    char,
    decimal,
    manyTill',
    parseOnly,
    sepBy,
    space,
 )
import Data.Either (rights)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

-- Types

type ConditionRecord = ([Symbol], [Int])
data Symbol = Damaged | Undamaged | Unknown deriving (Show, Ord, Enum, Eq)
type Memo = M.Map ConditionRecord Int

-- Functions
solution :: FilePath -> ([ConditionRecord] -> Int) -> IO Int
solution filePath valueFunc = do
    contents <- TIO.readFile filePath
    let conditionRecords = rights $ parseOnly parseLine <$> T.lines contents
    return $ valueFunc conditionRecords

sumArrangements :: [ConditionRecord] -> Int
sumArrangements crs = sum $ evalState (mapM countArrangements crs) M.empty

countArrangements :: ConditionRecord -> State Memo Int
countArrangements cr = do
    memo <- get
    case M.lookup cr memo of
        Just result -> return result
        Nothing -> do
            result <- go cr
            modify (M.insert cr result)
            return result
  where
    go :: ConditionRecord -> State Memo Int
    go ([], []) = return 1
    go ([], _) = return 0
    go (ss, []) = return $ if Damaged `elem` ss then 0 else 1
    go (Undamaged : rest, xs) = countArrangements (rest, xs)
    go (ss@(Damaged : _), x : xs)
        | length ss < x || Undamaged `elem` take x ss = return 0
        | otherwise = case drop x ss of
            [] -> countArrangements ([], xs)
            (Damaged : _) -> return 0
            (_ : rest) -> countArrangements (rest, xs)
    go (Unknown : rest, xs) = do
        countsWhenUndamaged <- countArrangements (Undamaged : rest, xs)
        countsWhenDamaged <- countArrangements (Damaged : rest, xs)
        return $ countsWhenUndamaged + countsWhenDamaged

-- Parsers

parseLine :: Parser ConditionRecord
parseLine = do
    symbols <- parseSymbols
    damagedGroups <- parseDamagedGroups
    return (symbols, damagedGroups)

parseSymbols :: Parser [Symbol]
parseSymbols = manyTill' parseSymbol space
  where
    parseSymbol = do
        c <- char '.' <|> char '#' <|> char '?'
        case c of
            '.' -> return Undamaged
            '#' -> return Damaged
            '?' -> return Unknown
            _ -> fail "Invalid symbol"

parseDamagedGroups :: Parser [Int]
parseDamagedGroups = decimal `sepBy` char ','
