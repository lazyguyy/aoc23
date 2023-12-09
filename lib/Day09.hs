{-# LANGUAGE OverloadedStrings, TupleSections #-}

module Day09 where

import qualified Data.Text as T

findDiffs :: [Int] -> [[Int]]
findDiffs = takeWhile (any (/= 0)) . iterate (\l -> zipWith (-) (tail l) $ l)

nextValues :: Int -> [Int] -> [Int]
nextValues n xs = continueList $ findDiffs xs
    where
        continueList []     = []
        continueList [a]    = replicate n $ head a
        continueList (a:as) = tail $ scanl (+) (last a) $ continueList as

part1 :: [[Int]] -> String
part1 = show . sum . map (head . nextValues 1)

part2 :: [[Int]] -> String
part2 = show . sum . map (head . nextValues 1 . reverse)

solve :: T.Text -> [String]
solve input = [part1 parsed, part2 parsed]
    where
        parsed = map (map read . map T.unpack . T.words) $ T.lines input