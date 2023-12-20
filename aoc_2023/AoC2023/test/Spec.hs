import Test.Hspec

import qualified Data.Set as S
import Day01.Part2
import Day03.Part1
import Day03.Part2
import Day04.Part1
import Day04.Part2
import Util

main :: IO ()
main = hspec $ do
    describe "findFirstAndLastDigit" $ do
        it "properly extracts first and last digits" $ do
            Day01.Part2.findFirstAndLastDigit "xtwone3four" `shouldBe` Just (2, 4)
            Day01.Part2.findFirstAndLastDigit "two1nine" `shouldBe` Just (2, 9)
            Day01.Part2.findFirstAndLastDigit "zoneight234" `shouldBe` Just (1, 4)
            Day01.Part2.findFirstAndLastDigit "four84" `shouldBe` Just (4, 4)
            Day01.Part2.findFirstAndLastDigit "8247819snr" `shouldBe` Just (8, 9)
            Day01.Part2.findFirstAndLastDigit "23eightptpspjtbnninesixfivedhfnmqjd" `shouldBe` Just (2, 5)
            Day01.Part2.findFirstAndLastDigit "ddgjgcrssevensix37twooneightgt" `shouldBe` Just (7, 8)

    describe "parseLine" $ do
        it "properly extracts Entities and their Coords" $ do
            Day03.Part1.parseLine 0 "...123...456.*" `shouldBe` Right [PartNum (S.fromList [(0, 3), (0, 4), (0, 5)]) 123, PartNum (S.fromList [(0, 9), (0, 10), (0, 11)]) 456, Symbol (S.fromList [(0, 13)]) "*"]

    describe "sumPartNums" $ do
        it "properly sums Part Numbers" $ do
            result <- Day03.Part1.solution "inputs/day03_test.txt" Day03.Part1.sumPartNums
            result `shouldBe` 4361

    describe "sumGearRatios" $ do
        it "properly sums gear ratios" $ do
            result <- Day03.Part1.solution "inputs/day03_test.txt" Day03.Part2.sumGearRatios
            result `shouldBe` 467835

    describe "scoreCards" $ do
        it "properly scores all Cards" $ do
            result <- Day04.Part1.solution "inputs/day04_test.txt" Day04.Part1.scoreCards
            result `shouldBe` 13

    describe "countCards" $ do
        it "properly counts all Cards" $ do
            result <- Day04.Part1.solution "inputs/day04_test.txt" Day04.Part2.countCards
            result `shouldBe` 30

    describe "floydTortoiseAndHare" $ do
        it "returns Nothing for an empty list" $ do
            floydTortoiseAndHare ([] :: [Int]) `shouldBe` Nothing

        it "returns Nothing for a single-element list" $ do
            floydTortoiseAndHare ([1] :: [Int]) `shouldBe` Nothing

        it "returns Nothing for a list without a cycle" $ do
            floydTortoiseAndHare ([1, 2] :: [Int]) `shouldBe` Nothing

        it "returns Nothing for a list with a cycle length 1" $ do
            floydTortoiseAndHare ([1, 1] :: [Int]) `shouldBe` Just (0, 1)

        it "returns the correct entry point and cycle length for an infinite list of just one element" $ do
            floydTortoiseAndHare (repeat 7 :: [Int]) `shouldBe` Just (0, 1)

        it "returns the correct entry point and cycle length for a list with prefix" $ do
            let listWithCycleAndTail = ([0, 1] ++ cycle [2, 3] :: [Int])
            floydTortoiseAndHare listWithCycleAndTail `shouldBe` Just (2, 2)

        it "returns the correct entry point and cycle length for a list without prefix" $ do
            let listWithOnlyCycle = (cycle [6, 2, 3, 1] :: [Int])
            floydTortoiseAndHare listWithOnlyCycle `shouldBe` Just (0, 4)
