module Day12.Part2 where

import Control.Monad.State
import qualified Data.List as L
import qualified Data.Map.Strict as M
import Day12.Part1

sumArrangements' :: [ConditionRecord] -> Int
sumArrangements' crs = sum $ evalState (mapM countArrangements (unfoldConditionRecord <$> crs)) M.empty

unfoldConditionRecord :: ConditionRecord -> ConditionRecord
unfoldConditionRecord (ss, xs) = (ss', xs')
  where
    ss' = L.intercalate [Unknown] $ replicate 5 ss
    xs' = concat $ replicate 5 xs
