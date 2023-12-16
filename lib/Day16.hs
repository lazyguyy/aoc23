{-# LANGUAGE TupleSections, OverloadedStrings #-}

module Day16 where

import qualified Data.Text as T
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Array as Array

import Llib (aocBlockInput, bfs)

data Element = Empty | DownMirror | UpMirror | VertSplitter | HoriSplitter deriving (Show)
type Coordinate = (Int, Int)
type Contraption = Array.Array Coordinate Element

data Direction = East | North | West | South deriving (Show, Enum, Eq, Ord)

prev :: Direction -> Direction
prev d = toEnum $ mod (fromEnum d - 1) 4

next :: Direction -> Direction
next d = toEnum $ mod (fromEnum d + 1) 4

data BfsState = State {coordinate :: Coordinate, direction ::  Direction} deriving (Show, Ord, Eq)

inBounds :: Contraption -> BfsState -> Bool
inBounds array (State (row, col) _) = and [1 <= row, row <= rows, 1 <= col, col <= cols]
    where
        (rows, cols) = snd $ Array.bounds array

move :: Coordinate -> Direction -> Coordinate
move (r, c) East = (r, c + 1)
move (r, c) North = (r - 1, c)
move (r, c) West = (r, c - 1)
move (r, c) South = (r + 1, c)

readElement :: Char -> Element
readElement '.' = Empty
readElement '\\' = DownMirror
readElement '/' = UpMirror
readElement '-' = VertSplitter
readElement '|' = HoriSplitter
readElement s = error $ s : " is no instance of Element"

nextSquares :: Contraption -> BfsState -> [BfsState]
nextSquares array (State coord dir) = case array Array.! coord of
    Empty -> [State (move coord dir) dir]
    VertSplitter -> if dir == North || dir == South then [State (move coord West) West, State (move coord East) East] else [State (move coord dir) dir]
    HoriSplitter -> if dir == West  || dir == East then [State (move coord North) North, State (move coord South) South] else [State (move coord dir) dir]
    DownMirror   -> if dir == East  || dir == West then [State (move coord $ prev dir) $ prev dir] else [State (move coord $ next dir) $ next dir]
    UpMirror     -> if dir == East  || dir == West then [State (move coord $ next dir) $ next dir] else [State (move coord $ prev dir) $ prev dir]

sendBeam :: Contraption -> BfsState -> Map.Map BfsState Int
sendBeam array start = bfs [start] (filter (inBounds array) . nextSquares array)

energizedTiles :: Contraption -> BfsState -> Int
energizedTiles array  = Set.size . Set.fromList . map coordinate . Map.keys . sendBeam array

part1 :: Contraption -> Int
part1 array = energizedTiles array (State (1, 1) East)

part2 :: Contraption -> Int
part2 array = maximum $ map (energizedTiles array) startPoints
    where
        (rows, cols) = snd $ Array.bounds array
        startPoints = [State (1, col) South    | col <- [1..cols]] ++
                      [State (rows, col) North | col <- [1..cols]] ++
                      [State (row, 1) East     | row <- [1..rows]] ++
                      [State (row, cols) West  | row <- [1..rows]]

solve :: T.Text -> [String]
solve text = [show $ part1 parsed, show $ part2 parsed]
    where
        parsed = aocBlockInput readElement text
