module Day06.Part2 (marginOfError') where

import Day06.Part1 (RaceRecord, marginOfError)

marginOfError' :: [(Int, Int)] -> Int
marginOfError' = marginOfError . concatRaceRecords

concatRaceRecords :: [RaceRecord] -> [RaceRecord]
concatRaceRecords races =
    let (times, distances) = unzip races
     in [(concatInts times, concatInts distances)]
  where
    concatInts = read . concatMap show
