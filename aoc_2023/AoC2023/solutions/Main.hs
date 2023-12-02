module Main (main) where

import Day01.Part1
import Day01.Part2
import Day02.Part1
import Day02.Part2

main :: IO ()
main = do
    putStrLn "Day 1, Part 1: " >> Day01.Part1.solution "inputs/day01.txt" Day01.Part1.findFirstAndLastDigit >>= print
    putStrLn "Day 1, Part 2: " >> Day01.Part1.solution "inputs/day01.txt" Day01.Part2.findFirstAndLastDigit >>= print
    putStrLn "Day 2, Part 1: " >> Day02.Part1.solution "inputs/day02.txt" Day02.Part1.evaluateGames >>= print
    putStrLn "Day 2, Part 2: " >> Day02.Part1.solution "inputs/day02.txt" Day02.Part2.evaluateGames >>= print
