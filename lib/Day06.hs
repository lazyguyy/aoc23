{-# LANGUAGE OverloadedStrings, TupleSections #-}

module Day06 where

import qualified Data.Text as T

import Text.Regex.Applicative
import Data.Char

data Race = Race {time :: Int, dist :: Int} deriving (Show)
data Range = Range {lower :: Int, upper :: Int} deriving (Show)
getDist :: Range -> Int
getDist (Range l u) = u - l + 1

emptyParser :: RE Char [Char]
emptyParser = many (sym ' ')

intParser :: RE Char Int
intParser = read <$> many (psym isNumber)

inputParser :: RE Char [Race]
inputParser = fmap (map makeRace) $ zip <$>
    (string "Time:" *> (many emptyParser) *> many (intParser <* optional emptyParser) <* sym '\n') <*>
    (string "Distance:" *> (many emptyParser) *> many (intParser <* optional emptyParser) <* many anySym)
    where
        makeRace :: (Int, Int) -> Race
        makeRace (t, d) = Race t d

combine :: [Race] -> Race
combine races = Race (read $ concat $ map (show . time) races) (read $ concat $ map (show . dist) races)

possibleTimes :: Race -> Range
possibleTimes (Race t d) = Range (if getDistance t lwr == d then lwr + 1 else lwr) (if getDistance t upr == d then upr - 1 else upr)
    where
        td = fromIntegral t
        root = sqrt $ ((td/2) ** 2) - (fromIntegral d)
        lwr = ceiling $ td/2 - root
        upr = floor $ td/2 + root

getDistance :: Int -> Int -> Int
getDistance total wait = (total - wait) * wait

part1 :: [Race] -> Int
part1 = product . map (getDist . possibleTimes)

part2 :: [Race] -> Int
part2 = getDist . possibleTimes . combine

solve :: T.Text -> [String]
solve text = [show $ fmap part1 parsed, show $ fmap part2 parsed]
    where
        parsed = (T.unpack text) =~ inputParser
