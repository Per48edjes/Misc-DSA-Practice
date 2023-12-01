{-# LANGUAGE OverloadedStrings #-}

module Day01.Part2 (findFirstAndLastDigit) where

import Control.Applicative
import Data.Attoparsec.Text
import Data.Char
import Data.Functor (($>))
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T

numeralWords :: [Text]
numeralWords = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

findFirstAndLastDigit :: Text -> Maybe (Int, Int)
findFirstAndLastDigit t = (,) <$> digitize (parseOnly (parseFirstNumeral numeralWordParser) t) <*> digitize (parseOnly (parseFirstNumeral numeralWordParserReverse) (T.reverse t))
  where
    wordToNumeral = zip (numeralWords <> (T.reverse <$> numeralWords)) (cycle [1 .. 9])
    digitize (Left _) = Nothing
    digitize (Right nums)
        | T.all isDigit (head nums) = Just $ read $ T.unpack (head nums)
        | otherwise = lookup (head nums) wordToNumeral

numeralWordParser :: Parser Text
numeralWordParser = choice (string <$> numeralWords)

numeralWordParserReverse :: Parser Text
numeralWordParserReverse = choice (string . T.reverse <$> numeralWords)

numeralLiteralParser :: Parser Text
numeralLiteralParser = T.singleton <$> digit

parseFirstNumeral :: Parser Text -> Parser [Text]
parseFirstNumeral wordParser = catMaybes <$> many' (Just <$> combinedParser <|> (anyChar $> Nothing))
  where
    combinedParser = numeralLiteralParser <|> wordParser
