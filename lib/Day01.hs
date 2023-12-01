{-# LANGUAGE OverloadedStrings, TupleSections #-}

module Day01 (readInput, part1, part2) where

import qualified Data.Text as T
import qualified Data.Text.IO as T
-- import qualified Data.Text.Read as T


import Data.Char (digitToInt, isDigit)
import Data.List.Extra (minimumOn, maximumOn)



digitStrings :: [T.Text]
digitStrings = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9"]


readInput :: FilePath -> IO [T.Text]
readInput = fmap (T.lines) . T.readFile

lineValue :: T.Text -> Int
lineValue line = (head digits) * 10 + (last digits)
    where
        digits = fmap digitToInt $ filter isDigit $ T.unpack line

findDigits :: T.Text -> [(Int, Int)]
findDigits text = concat $ map (\(i, d) -> map ((, mod (i+1) 10) . T.length . fst) $ T.breakOnAll d text) $ zip [0..] digitStrings


lineValue2 :: T.Text -> Int
lineValue2 line = firstDigit * 10 + lastDigit
    where
        digits = findDigits line
        firstDigit = snd $ minimumOn fst digits
        lastDigit = snd $ maximumOn fst digits

part1 :: [T.Text] -> Int
part1 = sum . fmap lineValue

part2 :: [T.Text] -> Int
part2 = sum . fmap lineValue2
