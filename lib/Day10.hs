{-# LANGUAGE TupleSections #-}

module Day10 where

import qualified Data.Text as T
import qualified Data.Map as Map
import qualified Data.Array as Array

import Data.List (find)
import Llib (forceMaybe)

type Position = (Int, Int)
type Distances = Map.Map Position Int
data Network = Network {rows :: Int, cols :: Int, plan :: Array.Array (Int, Int) Char} deriving (Show)

east :: Position
east = (0, 1)

north :: Position
north = (-1, 0)

west :: Position
west = (0, -1)

south :: Position
south = (1, 0)

connections :: Map.Map Char [Position]
connections = Map.fromList [('-', [west, east]), ('|', [north, south]), ('L', [north, east]), ('7', [west, south]), ('F', [south, east]), ('J', [north, west]), ('.', []), ('S', [north, east, west, south])]

canReach :: Network -> Position -> Position -> Bool
canReach n to from = elem to (adjacency n from)

add :: Position -> Position -> Position
add (r1, c1) (r2, c2) = (r1 + r2, c1 + c2)

inBounds :: Network -> Position -> Bool
inBounds n (r,c) = and [r >= 1, c >= 1, r <= rows n, c <= cols n]

adjacency :: Network -> Position -> [Position]
adjacency n@(Network _ _ m) p = filter (inBounds n) $ map (add p) $ connections Map.! (m Array.! p)

bfs :: Network -> Distances -> [Position] -> Distances
bfs _ d [] = d
bfs n d q = bfs n newDistances $ tail q ++ neighbors
    where
        neighbors = filter (not . (flip Map.member d)) $ filter (canReach n $ head q) $ adjacency n $ head q
        newDistances = foldl (\m nb -> Map.insert nb (1 + d Map.! (head q)) m) d neighbors

readInput :: String -> Network
readInput text = Network (length l) (length $ head l) $ Array.listArray ((1, 1), (length l, length $ head l)) $ concat l
    where
        l = lines text

getStartPos :: Network -> Position
getStartPos n = fst $ forceMaybe "No start symbol contained" $ find ((== 'S') . snd) $ Array.assocs $ plan n

part1 :: Network -> String
part1 n = show $ foldr max 0 $ bfs n (Map.fromList [(startPos, 0)]) [startPos]
    where
        startPos = getStartPos n

part2 :: Network -> String
part2 n = show $ sum $ map (enclosedInRow repaired distances) [1..(rows n)]
    where
        startPos = getStartPos n
        distances = bfs n (Map.fromList [(startPos, 0)]) [startPos]
        repaired = repairNetwork n

repairNetwork :: Network -> Network
repairNetwork n = Network (rows n) (cols n) $ plan n Array.// [(startPos, startTile)]
    where
        startPos = getStartPos n
        startAdjacency = filter (canReach n $ startPos) $ adjacency n startPos
        startTile = fst $ forceMaybe "No tile fits the start position" $ find (\(_, os) -> length os == 2 && (all (flip elem startAdjacency) $ map (add startPos) os)) $ Map.toList connections

enclosedInRow :: Network -> Distances -> Int -> Int
enclosedInRow n d row = countEnclosed $ map (\i -> if Map.member i d then plan n Array.! i else '.') $ zip (repeat row) [1..(cols n)]

countEnclosed :: [Char] -> Int
countEnclosed row = length $ filter (\(char, contained) -> char == '.' && contained) $ zip filtered $ scanl checkContained False $ zip filtered $ tail filtered
    where
        filtered = filter (/= '-') row
        checkContained contained ('|', _)   = not contained
        checkContained contained ('L', '7') = not contained
        checkContained contained ('F', 'J') = not contained
        checkContained contained _          = contained

solve :: T.Text -> [String]
solve text = [part1 network, part2 network]
    where
        network = readInput $ T.unpack text