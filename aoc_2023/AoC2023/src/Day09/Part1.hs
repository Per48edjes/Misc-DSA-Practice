{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Day09.Part1 (solution, sumExtrapolations, diffSeq) where

import Data.Sequence (Seq ((:|>)), (|>))
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Read as TR

solution :: FilePath -> ([Seq Int] -> Int) -> IO Int
solution filePath valueFunc = do
    contents <- TIO.readFile filePath
    let intSeqs = fmap parseTextToInt . Seq.fromList . T.words <$> T.lines contents
    return $ valueFunc intSeqs

sumExtrapolations :: [Seq Int] -> Int
sumExtrapolations = sum . fmap (getExtrapolatedValue . extrapolate . diffSeq)

getExtrapolatedValue :: [Seq Int] -> Int
getExtrapolatedValue (s : _) = case s of
    (_ :|> lastS) -> lastS

extrapolate :: [Seq Int] -> [Seq Int]
extrapolate [] = []
extrapolate (s@(_ :|> lastS) : ss) = case extrapolate ss of
    diffed@((_ :|> lastS') : _) -> (s |> lastS + lastS') : diffed
    [] -> [s]

diff :: Seq Int -> Seq Int
diff s = Seq.zipWith (-) (Seq.drop 1 s) s

diffSeq :: Seq Int -> [Seq Int]
diffSeq s
    | all (== 0) s = [s]
    | otherwise = s : diffSeq (diff s)

-- Parsers

parseTextToInt :: T.Text -> Int
parseTextToInt txt =
    let int = case TR.signed TR.decimal txt of
            Right (i, _) -> i
            Left _ -> error "Could not parse text to int"
     in int
