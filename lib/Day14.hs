{-# LANGUAGE TupleSections, OverloadedStrings #-}

module Day14 where

import qualified Data.Text as T
import qualified Data.Array as Array
import qualified Data.Set as Set
import qualified Data.Map as Map

import Data.Maybe (fromMaybe)
import Data.List (transpose)
import Llib (aocBlockInput, arrayByRow)

data Space = Empty | Rock | Boulder deriving (Eq, Ord)
instance Show Space where
    show Empty = "."
    show Rock = "#"
    show Boulder = "O"

readSpace :: Char -> Space
readSpace '#' = Rock
readSpace 'O' = Boulder
readSpace _   = Empty

type Platform = Array.Array (Int, Int) Space
type Coord = (Int, Int)

readInput :: T.Text -> Platform
readInput = aocBlockInput readSpace

-- moves all the stuff on the axis specified by the coordinates of its points to its front
tiltAxis :: Platform -> [Coord] -> [Space]
tiltAxis spaces axis = moveAlongAxis spaces rockIndices axis 1 1
    where
        rockIndices = Set.fromList $ map fst $ filter ((== Rock) . (spaces Array.!) . snd) $ zip [1..] axis

moveAlongAxis :: Platform -> Set.Set Int -> [Coord] -> Int -> Int -> [Space]
moveAlongAxis _ rocks [] offset index = replicate (index - (fromMaybe 0 $ Set.lookupLT index rocks) - offset) Empty
moveAlongAxis spaces rocks (c:cs) offset index = case spaces Array.! c of
    Empty   -> moveAlongAxis spaces rocks cs offset $ index + 1
    Boulder -> Boulder : (moveAlongAxis spaces rocks cs (offset + 1) $ index + 1)
    Rock    -> replicate (index - leftmostRock - offset) Empty ++ (Rock : moveAlongAxis spaces rocks cs 1 (index + 1))
    where
        leftmostRock = (fromMaybe 0 $ Set.lookupLT index rocks)

tiltNorth :: Platform -> Platform
tiltNorth spaces = Array.listArray (Array.bounds spaces) $ concat $ transpose $ map (tiltAxis spaces) [zip [1..rows] (repeat col) | col <- [1..cols]]
    where
        (rows, cols) = snd $ Array.bounds spaces

tiltSouth :: Platform -> Platform
tiltSouth spaces = Array.listArray (Array.bounds spaces) $ concat $ transpose $ map (reverse . tiltAxis spaces) [zip (reverse [1..rows]) (repeat col) | col <- [1..cols]]
    where
        (rows, cols) = snd $ Array.bounds spaces

tiltWest :: Platform -> Platform
tiltWest spaces = Array.listArray (Array.bounds spaces) $ concat $ map (tiltAxis spaces) [zip (repeat row) [1..cols] | row <- [1..rows]]
    where
        (rows, cols) = snd $ Array.bounds spaces


tiltEast :: Platform -> Platform
tiltEast spaces = Array.listArray (Array.bounds spaces) $ concat $ map (reverse . tiltAxis spaces) [zip (repeat row) (reverse [1..cols]) | row <- [1..rows]]
    where
        (rows, cols) = snd $ Array.bounds spaces

getLoad :: [[Space]] -> Int
getLoad spaces = sum $ zipWith (*) [1..] $ reverse $ map (length . filter (== Boulder)) spaces


spin :: Platform -> Platform
spin = tiltEast . tiltSouth . tiltWest . tiltNorth

part1 :: Platform -> String
part1 spaces = show $ getLoad $ arrayByRow $ tiltNorth spaces

part2 :: Platform -> Int -> String
part2 spaces n = show $ getLoad $ arrayByRow $ spinResults !! index
    where
        spinResults = iterate spin spaces
        (offset, len) = findCycle spinResults Map.empty 0
        index = if n <= offset then n else (offset + mod (n - offset) len)

findCycle :: [Platform] -> Map.Map Platform Int -> Int -> (Int, Int)
findCycle (p:ps) m index = case Map.lookup p m of
    Nothing  -> findCycle ps (Map.insert p index m) $ index + 1
    Just app -> (app, index - app)

solve :: T.Text -> [String]
solve text = [part1 input, part2 input 1000000000]
    where
        input = readInput text
