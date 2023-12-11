{-# LANGUAGE TupleSections #-}

module Day11 (solve) where

import qualified Data.Text as T
import qualified Data.Set as Set
import qualified Data.Array as Array

import Data.Maybe (fromMaybe)

type Coord = (Int, Int)
type Observation = Array.Array Coord Char


data Space = Space {observed :: Observation, emptyRows :: Set.Set Int, emptyCols :: Set.Set Int} deriving (Show)

readInput :: T.Text -> Space
readInput text = Space observedSpace (getEmptyRows observedSpace) $ getEmptyCols observedSpace
    where
        l = lines $ T.unpack text
        rows = length $ l
        cols = length $ head $ l
        observedSpace = Array.listArray ((1, 1), (rows, cols)) $ concat l

getEmptyRows :: Observation -> Set.Set Int
getEmptyRows obs = Set.fromList $ filter (\r -> not $ any (=='#') $ map (obs Array.!) $ zip (repeat r) [1..cols] ) [1..rows]
    where
        rows = fst $ snd $ Array.bounds obs
        cols = snd $ snd $ Array.bounds obs

getEmptyCols :: Observation -> Set.Set Int
getEmptyCols obs = Set.fromList $ filter (\c -> not $ any (=='#') $ map (obs Array.!) $ zip [1..rows] (repeat c)) [1..cols]
    where
        rows = fst $ snd $ Array.bounds obs
        cols = snd $ snd $ Array.bounds obs

findGalaxies :: Observation -> [Coord]
findGalaxies obs = map fst $ filter ((=='#') . snd) $ Array.assocs obs

bisectLeft :: Set.Set Int -> Int -> Int
bisectLeft set i = fromMaybe 0 result
    where
        result = Set.lookupLE i set >>= fmap (+1) . flip Set.lookupIndex set

getDistance :: Int -> Set.Set Int -> Set.Set Int -> Coord -> Coord -> Int
getDistance scale eRows eCols (r1, c1) (r2, c2) = horizontal + vertical
    where
        horizontal = abs (c2 - c1) + (scale - 1) * ((bisectLeft eCols $ max c2 c1) - (bisectLeft eCols $ min c2 c1))
        vertical   = abs (r2 - r1) + (scale - 1) * ((bisectLeft eRows $ max r2 r1) - (bisectLeft eRows $ min r2 r1))

part1 :: Space -> String
part1 s = show $ flip div 2 $ sum $ map (uncurry $ getDistance 2 (getEmptyRows $ observed s) (getEmptyCols $ observed s)) galaxyPairs
    where
        galaxies = findGalaxies $ observed s
        galaxyPairs = (,) <$> galaxies <*> galaxies

part2 :: Space -> String
part2 s = show $ flip div 2 $ sum $ map (uncurry $ getDistance 1000000 (getEmptyRows $ observed s) (getEmptyCols $ observed s)) galaxyPairs
    where
        galaxies = findGalaxies $ observed s
        galaxyPairs = (,) <$> galaxies <*> galaxies

solve :: T.Text -> [String]
solve text = [part1 input, part2 input]
    where
        input = readInput text