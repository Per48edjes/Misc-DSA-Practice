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

main :: IO ()
main = do
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
