{-# LANGUAGE TupleSections, OverloadedStrings #-}

module Day17 where

import qualified Data.Text as T
import qualified Data.Map as Map
import qualified Data.Array as Array
-- import qualified Data.HashMap.Strict as HMap
import qualified Data.PQueue.Min as PQ

import Llib (aocBlockInput)
import Data.Char (digitToInt)

type Coordinate = (Int, Int)
type Network = Array.Array Coordinate Int
data DijkstraInput = Input {array :: Network, minSteps :: Int, maxSteps :: Int}

data Direction = North | East | South | West | None deriving (Show, Enum, Eq, Ord)

data NetworkNode = Node {coordinate :: Coordinate, lastDirection :: Direction} deriving (Show, Eq, Ord)
type PathMapping = Map.Map NetworkNode NetworkNode

move :: Coordinate -> Direction -> Coordinate
move (r, c) North = (r - 1, c)
move (r, c) South = (r + 1, c)
move (r, c) East  = (r, c + 1)
move (r, c) West  = (r, c - 1)
move c None = c

opposite :: Direction -> Direction -> Bool
opposite dir1 dir2 = dir1 /= None && dir2 /= None && abs (fromEnum dir1 - fromEnum dir2) == 2

inBounds :: Network -> Coordinate -> Bool
inBounds arr (row, col) = and [1 <= row, row <= rows, 1 <= col, col <= cols]
    where
        (rows, cols) = snd $ Array.bounds arr

directionalNeighbors :: DijkstraInput -> NetworkNode -> Direction -> [(Int, NetworkNode)]
directionalNeighbors input (Node coord trackedDirection) direction = if direction == trackedDirection || opposite direction trackedDirection
    then []
    else drop (minSteps input - 1) $ getNext (move coord direction) (maxSteps input) 0
    where
        getNext :: Coordinate -> Int -> Int -> [(Int, NetworkNode)]
        getNext _ 0 _ = []
        getNext current n heat = if inBounds (array input) current then (heat + ((array input) Array.! current), Node current direction) : (getNext (move current direction) (n - 1) $ heat + ((array input) Array.! current)) else []

getNeighbors :: DijkstraInput -> NetworkNode -> [(Int, NetworkNode)]
getNeighbors input node = concat $ map (directionalNeighbors input node) [North, East, South, West]

dijkstra :: DijkstraInput -> Coordinate -> Coordinate -> Maybe Int
dijkstra input from to = fst <$> result
    where
        (_, result) = dijkstraStep input ((0, Node from None, Nothing) PQ.:< PQ.Empty) Map.empty to

dijkstraStep :: DijkstraInput -> PQ.MinQueue (Int, NetworkNode, Maybe NetworkNode) -> PathMapping -> Coordinate -> (PathMapping, Maybe (Int, NetworkNode))
dijkstraStep _ PQ.Empty mapping _ = (mapping, Nothing)
dijkstraStep input ((distance, node, prev) PQ.:< withoutMin) mapping target = case (Map.member node mapping, target == coordinate node) of
    (_, True) -> (updatedPathMapping, Just (distance, node))
    (True, _) -> dijkstraStep input withoutMin mapping target
    _         -> dijkstraStep input updatedQueue updatedPathMapping target
    where
        updatedPathMapping = case prev of
            Nothing -> mapping
            Just p  -> Map.insert node p mapping
        neighbors    = getNeighbors input node
        updatedQueue = foldl (\q (nodeDist, next) -> PQ.insert (distance + nodeDist, next, Just node) q) withoutMin neighbors

getPath :: PathMapping -> NetworkNode -> [Coordinate]
getPath mapping current = if not $ Map.member current mapping then [] else (coordinate current) : getPath mapping (mapping Map.! current)

part1 :: Network -> String
part1 network = show $ dijkstra (Input network 1 3) (1, 1) target
    where
        target = snd $ Array.bounds network

part2 :: Network -> String
part2 network = show $ dijkstra (Input network 4 10) (1, 1) target
    where
        target = snd $ Array.bounds network


solve :: T.Text -> [String]
solve text = [part1 parsed, part2 parsed]
    where
        parsed = aocBlockInput digitToInt text