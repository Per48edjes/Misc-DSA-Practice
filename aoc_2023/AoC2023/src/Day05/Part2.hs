module Day05.Part2 (minLocation') where

import Data.Sequence (Seq, (><))
import qualified Data.Sequence as Seq
import Day05.Part1 (Interval, Location, Mapping, mappingToMappingFunc, seedToLocationGen)

type Range = (Int, Int)

minLocation' :: [Mapping] -> [Int] -> Location
minLocation' mappings xs = minimum $ map (getMinBound . seedToLocationGen mappings mappingToMappingFunc') seedRanges
  where
    seedRanges = map (Seq.singleton . intervalToRange) $ pairSuccessive xs
    getMinBound = minimum . fmap fst

-- Analogue to mappingToMappingFunc, producing a function from a Seq Range to Seq Range
-- rather than Int to Int
mappingToMappingFunc' :: Mapping -> (Seq Range -> Seq Range)
mappingToMappingFunc' mapping = mappingFunc
  where
    mappingFunc domainRanges = getCodomainRange <$> domainRanges'
      where
        domainComparisonRanges = domainIntervalToRange <$> mapping
        domainRanges' = foldl divideRange domainRanges domainComparisonRanges
        getCodomainRange (l, r) =
            let f = mappingToMappingFunc mapping
             in (f l, f r)

-- Takes a Seq of Ranges and a comparison Range, splits the former into a flattened Seq of Ranges
-- that are either disjoint to the comparison Range or are a subset of the comparison Range
divideRange :: Seq Range -> Range -> Seq Range
divideRange acc comparisonRange = foldl (><) Seq.empty $ do
    range <- acc
    return $ rangeSplit range comparisonRange

-- Convert an Interval to source Range
domainIntervalToRange :: Interval -> Range
domainIntervalToRange (_, srcStart, rangeLen) = intervalToRange (srcStart, rangeLen)

intervalToRange :: (Int, Int) -> Range
intervalToRange (x, len) = (x, x + len - 1)

-- Take a Range and a comparison Range, split the former into two Ranges if it overlaps the latter
rangeSplit :: Range -> Range -> Seq Range
rangeSplit (l1, r1) (l2, r2)
    | l1 < l2 && r1 > l2 && r1 < r2 = Seq.fromList [(l1, l2 - 1), (l2, r1)]
    | l1 > l2 && l1 < r2 && r1 > r2 = Seq.fromList [(l1, r2), (r2 + 1, r1)]
    | l1 < l2 && r1 > r2 = Seq.fromList [(l1, l2 - 1), (l2, r2), (r2 + 1, r1)]
    | otherwise = Seq.singleton (l1, r1)

pairSuccessive :: [a] -> [(a, a)]
pairSuccessive (x : y : zs) = (x, y) : pairSuccessive zs
pairSuccessive _ = []
