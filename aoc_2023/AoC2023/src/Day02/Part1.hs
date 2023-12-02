module Day02.Part1 (solution, evaluateGames, parseGame, Color (..)) where

import Data.Attoparsec.Text
import Data.Char (isDigit)
import Data.Functor (($>))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

solution :: FilePath -> (Text -> Int) -> IO Int
solution filePath valueFunc = do
    contents <- TIO.readFile filePath
    return $ sum $ valueFunc <$> T.lines contents

data Color = Red | Green | Blue deriving (Show, Eq, Enum)

cubes :: [(Color, Int)]
cubes = [(Red, 12), (Green, 13), (Blue, 14)]

evaluateGames :: Text -> Int
evaluateGames line
    | and (fromMaybe [False] (traverse colorPossibility cubeCounts)) = gameNumber
    | otherwise = 0
  where
    (gameNumber, cubeCounts) = case parseOnly parseGame line of
        Left _ -> error "Invalid input!"
        Right parsed -> parsed

colorPossibility :: (Color, Int) -> Maybe Bool
colorPossibility (c, n) = (<=) n <$> (c `lookup` cubes)

-- Parsers

parseGameNumber :: Parser Int
parseGameNumber = skipWhile (not . isDigit) *> decimal

parseColorNumber :: Parser (Color, Int)
parseColorNumber = do
    skipWhile (not . isDigit)
    n <- decimal
    c <- choice [string " red" $> Red, string " green" $> Green, string " blue" $> Blue]
    return (c, n)

parseGame :: Parser (Int, [(Color, Int)])
parseGame = do
    gameNumber <- parseGameNumber
    cubeCounts <- many' parseColorNumber
    return (gameNumber, cubeCounts)
