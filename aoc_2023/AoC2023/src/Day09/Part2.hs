{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Day09.Part2 (sumExtrapolations', extrapolate', getExtrapolatedValue') where

import Data.Sequence (Seq ((:<|)), (<|))
import Day09.Part1 (diffSeq)

sumExtrapolations' :: [Seq Int] -> Int
sumExtrapolations' = sum . fmap (getExtrapolatedValue' . extrapolate' . diffSeq)

getExtrapolatedValue' :: [Seq Int] -> Int
getExtrapolatedValue' (s : _) = case s of
    (firstS :<| _) -> firstS

extrapolate' :: [Seq Int] -> [Seq Int]
extrapolate' [] = []
extrapolate' (s@(firstS :<| _) : ss) = case extrapolate' ss of
    diffed@((firstS' :<| _) : _) -> (firstS - firstS' <| s) : diffed
    [] -> [s]
