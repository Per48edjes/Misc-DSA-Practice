{-# LANGUAGE OverloadedStrings #-}

module Day01.Part1 (solution, findFirstAndLastDigit) where

import Data.Char
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.IO

solution :: FilePath -> (Text -> Maybe (Int, Int)) -> IO Int
solution filePath valueParser = do
    withFile filePath ReadMode $ \handle -> do
        contents <- TIO.hGetContents handle
        return $ sum $ lineValue valueParser <$> T.lines contents

lineValue :: (Text -> Maybe (Int, Int)) -> Text -> Int
lineValue valueParser = maybe 0 (uncurry concatDigits) . valueParser
  where
    concatDigits a b = a * 10 + b

findFirstAndLastDigit :: Text -> Maybe (Int, Int)
findFirstAndLastDigit line = case T.filter isDigit line of
    "" -> Nothing
    digits -> Just (digitToInt $ T.head digits, digitToInt $ T.last digits)
