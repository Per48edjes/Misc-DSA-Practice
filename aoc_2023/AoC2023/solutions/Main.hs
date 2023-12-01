module Main (main) where

import Day01.Part1
import Day01.Part2

main :: IO ()
main = do
    putStrLn "Day 1, Part 1: " >> Day01.Part1.solution "inputs/day01.txt" Day01.Part1.findFirstAndLastDigit >>= print
    putStrLn "Day 1, Part 2: " >> Day01.Part1.solution "inputs/day01.txt" Day01.Part2.findFirstAndLastDigit >>= print
