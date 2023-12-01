import Test.Hspec

import Day01.Part2

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
