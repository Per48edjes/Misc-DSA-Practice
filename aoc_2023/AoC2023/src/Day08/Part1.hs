{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Day08.Part1 (solution, countMoves, processMove, extractData, Move, Node) where

import Control.Monad.Extra
import Control.Monad.State
import Data.Attoparsec.Text
import Data.Char (isAlpha)
import Data.Either (rights)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

type Node = Text
type Move = Text

solution :: FilePath -> ([Move] -> [(Node, (Node, Node))] -> Int) -> IO Int
solution filePath valueFunc = do
    contents <- TIO.readFile filePath
    let (moveSequence, nodeAdjacencies) = extractData contents
    return $ valueFunc moveSequence nodeAdjacencies

countMoves :: [Move] -> [(Node, (Node, Node))] -> Int
countMoves moveSequence nodeAdjacencies =
    let (_, moveCount) = execState (anyM (processMove (== "ZZZ") adjacencyMap) $ cycle moveSequence) ("AAA", 0)
     in moveCount
  where
    adjacencyMap = M.fromList nodeAdjacencies

processMove :: (Node -> Bool) -> M.Map Node (Node, Node) -> Move -> State (Node, Int) Bool
processMove stopCondition adjacencyMap move = do
    (node, moveCount) <- get
    let (nextLeft, nextRight) = adjacencyMap M.! node
    if stopCondition node && moveCount > 0
        then return True
        else put (if move == "L" then (nextLeft, moveCount + 1) else (nextRight, moveCount + 1)) >> return False

-- Parsers

extractData :: Text -> ([Move], [(Node, (Node, Node))])
extractData contents =
    let (moveText, rest) = case break T.null $ T.lines contents of
            (mT : _, r) -> (mT, r)
            _ -> error "Failed to parse input"
        moveSequence = case parseOnly parseMoveSequence moveText of
            Left _ -> error "Failed to parse move sequence"
            Right ms -> ms
        nodeAdjacencies = rights $ map (parseOnly parseNodeAdjacencies) $ dropWhile T.null rest
     in (moveSequence, nodeAdjacencies)

parseMoveSequence :: Parser [Text]
parseMoveSequence = many1 parseMove
  where
    parseMove = choice [string "L", string "R"]

parseNodeAdjacencies :: Parser (Text, (Text, Text))
parseNodeAdjacencies = do
    u <- parseNode
    string " = "
    skip (== '(')
    vL <- parseNode
    string ", "
    vR <- parseNode
    skip (== ')')
    return (T.pack u, (T.pack vL, T.pack vR))
  where
    parseNode = count 3 $ satisfy isAlpha
