module Day15.Part1 (solution, hash, sumResults) where

import Data.Attoparsec.Text
import Data.Char (ord)
import qualified Data.List as L
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

solution :: FilePath -> ([Text] -> Int) -> IO Int
solution filePath valueFunc = do
  contents <- TIO.readFile filePath
  let steps = case parseOnly parseSteps contents of
        Left err -> error err
        Right s -> s
  return $ valueFunc steps

sumResults :: [Text] -> Int
sumResults = sum . map hash

hash :: Text -> Int
hash = L.foldl' processChar 0 . T.unpack
 where
  processChar acc c = (acc + ord c) * 17 `mod` 256

-- Parsers

parseSteps :: Parser [Text]
parseSteps = takeTill (\c -> c == ',' || c == '\n') `sepBy` char ','
