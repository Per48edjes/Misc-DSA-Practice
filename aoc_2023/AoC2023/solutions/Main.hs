module Main (main) where

import Day01.Part1
import Day01.Part2
import Day02.Part1
import Day02.Part2
import Day03.Part1
import Day03.Part2
import Day04.Part1
import Day04.Part2
import Day05.Part1
import Day05.Part2
import Day06.Part1
import Day06.Part2
import Day07.Part1
import Day07.Part2
import Day08.Part1
import Day08.Part2
import Day09.Part1
import Day09.Part2
import Day10.Part1
import Day10.Part2
import Day11.Part1
import Day11.Part2
import Day12.Part1
import Day12.Part2
import Day13.Part1
import Day13.Part2
import Day14.Part1
import Day14.Part2
import Day15.Part1
import Day15.Part2
import Day16.Part1
import Day16.Part2
import Day17.Part1
import Day17.Part2
import Day18.Part1

main :: IO ()
main = do
    {-
        putStrLn "Day 1, Part 1: " >> Day01.Part1.solution "inputs/day01.txt" Day01.Part1.findFirstAndLastDigit >>= print
        putStrLn "Day 1, Part 2: " >> Day01.Part1.solution "inputs/day01.txt" Day01.Part2.findFirstAndLastDigit >>= print
        putStrLn "Day 2, Part 1: " >> Day02.Part1.solution "inputs/day02.txt" Day02.Part1.evaluateGames >>= print
        putStrLn "Day 2, Part 2: " >> Day02.Part1.solution "inputs/day02.txt" Day02.Part2.evaluateGames >>= print
        putStrLn "Day 3, Part 1: " >> Day03.Part1.solution "inputs/day03.txt" Day03.Part1.sumPartNums >>= print
        putStrLn "Day 3, Part 2: " >> Day03.Part1.solution "inputs/day03.txt" Day03.Part2.sumGearRatios >>= print
        putStrLn "Day 4, Part 1: " >> Day04.Part1.solution "inputs/day04.txt" Day04.Part1.scoreCards >>= print
        putStrLn "Day 4, Part 2: " >> Day04.Part1.solution "inputs/day04.txt" Day04.Part2.countCards >>= print
        putStrLn "Day 5, Part 1: " >> Day05.Part1.solution "inputs/day05.txt" Day05.Part1.minLocation >>= print
        putStrLn "Day 5, Part 2: " >> Day05.Part1.solution "inputs/day05.txt" Day05.Part2.minLocation' >>= print
        putStrLn "Day 6, Part 1: " >> Day06.Part1.solution "inputs/day06.txt" Day06.Part1.marginOfError >>= print
        putStrLn "Day 6, Part 2: " >> Day06.Part1.solution "inputs/day06.txt" Day06.Part2.marginOfError' >>= print
        putStrLn "Day 7, Part 1: " >> Day07.Part1.solution "inputs/day07.txt" Day07.Part1.evaluateHands >>= print
        putStrLn "Day 7, Part 2: " >> Day07.Part1.solution "inputs/day07.txt" Day07.Part2.evaluateHands' >>= print
        putStrLn "Day 8, Part 1: " >> Day08.Part1.solution "inputs/day08.txt" Day08.Part1.countMoves >>= print
        putStrLn "Day 8, Part 2: " >> Day08.Part1.solution "inputs/day08.txt" Day08.Part2.countMoves' >>= print
        putStrLn "Day 9, Part 1: " >> Day09.Part1.solution "inputs/day09.txt" Day09.Part1.sumExtrapolations >>= print
        putStrLn "Day 9, Part 2: " >> Day09.Part1.solution "inputs/day09.txt" Day09.Part2.sumExtrapolations' >>= print
        putStrLn "Day 10, Part 1: " >> Day10.Part1.solution "inputs/day10.txt" Day10.Part1.findLoopMidpointDistance >>= print
        putStrLn "Day 10, Part 2: " >> Day10.Part1.solution "inputs/day10.txt" Day10.Part2.countLoopContainedElements >>= print
        putStrLn "Day 11, Part 1: " >> Day11.Part1.solution "inputs/day11.txt" Day11.Part1.sumGalaxyAPSPs >>= print
        putStrLn "Day 11, Part 2: " >> Day11.Part1.solution "inputs/day11.txt" Day11.Part2.sumGalaxyAPSPs' >>= print
        putStrLn "Day 12, Part 1: " >> Day12.Part1.solution "inputs/day12.txt" Day12.Part1.sumArrangements >>= print
        putStrLn "Day 12, Part 2: " >> Day12.Part1.solution "inputs/day12.txt" Day12.Part2.sumArrangements' >>= print
        putStrLn "Day 13, Part 1: " >> Day13.Part1.solution "inputs/day13.txt" Day13.Part1.summarizePatterns >>= print
        putStrLn "Day 13, Part 2: " >> Day13.Part1.solution "inputs/day13.txt" Day13.Part2.summarizePatterns' >>= print
        putStrLn "Day 14, Part 1: " >> Day14.Part1.solution "inputs/day14.txt" Day14.Part1.calculateLoad >>= print
        putStrLn "Day 14, Part 2: " >> Day14.Part1.solution "inputs/day14.txt" Day14.Part2.calculateLoad' >>= print
        putStrLn "Day 15, Part 1: " >> Day15.Part1.solution "inputs/day15.txt" Day15.Part1.sumResults >>= print
        putStrLn "Day 15, Part 2: " >> Day15.Part1.solution "inputs/day15.txt" Day15.Part2.totalFocusingPower >>= print
        putStrLn "Day 16, Part 1: " >> Day16.Part1.solution "inputs/day16.txt" Day16.Part1.countEnergizedTiles >>= print
        putStrLn "Day 16, Part 2: " >> Day16.Part1.solution "inputs/day16.txt" Day16.Part2.countEnergizedTiles' >>= print
        putStrLn "Day 17, Part 1: " >> Day17.Part1.solution "inputs/day17.txt" Day17.Part1.shortestPathFromTopLeft >>= print
        putStrLn "Day 17, Part 2: " >> Day17.Part1.solution "inputs/day17.txt" Day17.Part2.shortestPathFromTopLeft' >>= print
    -}
    putStrLn "Day 18, Part 1: " >> Day18.Part1.solution "inputs/day18_test.txt" Day18.Part1.totalTrenchVolume >>= print
