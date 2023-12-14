{-# LANGUAGE TupleSections, OverloadedStrings #-}

module Day13 where

import qualified Data.Text as T
import qualified Data.Array as Array

import Llib (aocBlockInput)

import Data.Bits ((.&.), xor)

type Reading = Array.Array (Int, Int) Char

data ConvertedReading = Converted {rowData :: [Integer], colData :: [Integer]} deriving (Show)

convertReading :: Reading -> ConvertedReading
convertReading r = Converted (map (asBinary . getRowValues r) [1..rows]) (map (asBinary . getColValues r) [1..cols])
    where
        (rows, cols) = snd $ Array.bounds r
        asBinary = foldl (\b a -> 2*b + (if a == '#' then 1 else 0)) 0

getRowValues :: Array.Array (Int, Int) a  -> Int -> [a]
getRowValues arr row = map (arr Array.!) $ zip (repeat row) [1..cols]
    where
        (_, cols) = snd $ Array.bounds arr

getColValues :: Array.Array (Int, Int) a  -> Int -> [a]
getColValues arr col = map (arr Array.!) $ zip [1..rows] (repeat col)
    where
        (rows, _) = snd $ Array.bounds arr

parseInput :: T.Text -> [ConvertedReading]
parseInput text = map (convertReading . aocBlockInput id) $ T.splitOn "\n\n" text

findAxis :: [Integer] -> [Integer] -> Int
findAxis [] _ = 0
findAxis (b:bs) [] = findAxis bs [b]
findAxis back@(b:bs) front = if prefixMatch back front then length front else findAxis bs (b:front)
    where
        prefixMatch _ [] = True
        prefixMatch [] _ = True
        prefixMatch (x:xs) (y:ys) = x == y && prefixMatch xs ys

valueOf :: ConvertedReading -> Int
valueOf cr = findAxis (colData cr) [] + (100 * findAxis (rowData cr) [])

isPowerOf2 :: Integer -> Bool
isPowerOf2 n = n .&. (n-1) == 0

prefixDiffs :: [Integer] -> [Integer] -> [Integer]
prefixDiffs _ [] = []
prefixDiffs [] _ = []
prefixDiffs (x:xs) (y:ys) = (xor x y) : prefixDiffs xs ys

isSmudge :: [Integer] -> Bool
isSmudge diffs = length nonzero == 1 && (isPowerOf2 $ head nonzero)
    where
        nonzero = filter (/=0) diffs

findSmudgedAxis :: [Integer] -> [Integer] -> Int
findSmudgedAxis [] _ = 0
findSmudgedAxis (b:bs) [] = findSmudgedAxis bs [b]
findSmudgedAxis back@(b:bs) front = if isSmudge $ prefixDiffs back front then length front else findSmudgedAxis bs (b:front)

smudgeValueOf :: ConvertedReading -> Int
smudgeValueOf cr = findSmudgedAxis (colData cr) [] + (100 * findSmudgedAxis (rowData cr) [])

part1 :: [ConvertedReading] -> String
part1 = show . sum . map valueOf

part2 :: [ConvertedReading] -> String
part2 = show . sum . map smudgeValueOf

solve :: T.Text -> [String]
solve text = [part1 parsed, part2 parsed]
    where
        parsed = parseInput text