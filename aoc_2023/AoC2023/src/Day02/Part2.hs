module Day02.Part2 (evaluateGames) where

import Data.Attoparsec.Text
import Data.List (maximumBy)
import Data.Ord (comparing)
import Data.Text (Text)
import Day02.Part1 (Color (..), parseGame)

maxCubeCounts :: Color -> [(Color, Int)] -> (Color, Int)
maxCubeCounts color cubeCounts = maximumBy (comparing snd) onlyColor
  where
    onlyColor = filter (\(c, _) -> c == color) cubeCounts

evaluateGames :: Text -> Int
evaluateGames line = product $ snd <$> [maxCubeCounts color cubeCounts | color <- [Red .. Blue]]
  where
    (_, cubeCounts) = case parseOnly parseGame line of
        Left _ -> error "Invalid input!"
        Right parsed -> parsed
