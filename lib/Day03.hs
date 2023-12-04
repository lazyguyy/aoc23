{-# LANGUAGE OverloadedStrings, TupleSections #-}

module Day03 (readInput, part1, part2) where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Set as Set
import qualified Data.Map as Map

import Text.Regex.Applicative
import Data.Char

type Coordinate = (Int, Int)

data Symbol = Symbol {char :: Char, symRow :: Int, symCol :: Int} deriving (Show)
instance Ord Symbol where
    compare (Symbol _ r1 c1) (Symbol _ r2 c2) = compare (r1, c1) (r2, c2)
instance Eq Symbol where
    (==) (Symbol _ r1 c1) (Symbol _ r2 c2) = (r1, c1) == (r2, c2)
type SymbolSet = Set.Set Symbol
data Part = Part {num :: String, partRow :: Int, partCol :: Int} deriving (Show)

data Schematic = Schematic SymbolSet [Part] deriving (Show)

readInput :: FilePath -> IO (Maybe Schematic)
readInput = fmap (makeSchematic . T.lines) . T.readFile

makeSchematic :: [T.Text] -> Maybe Schematic
makeSchematic text = Schematic <$> (Set.fromList <$> symbols text) <*> parts text
    where
        parts :: [T.Text] -> Maybe [Part]
        parts = (concat <$>) . sequence . map ((\(row, line) -> (zip [1..] (T.unpack line)) =~ (partParser row))) . zip [1..]
        symbols = (concat <$>) . sequence . map ((\(row, line) -> (zip [1..] (T.unpack line)) =~ (symbolParser row))) . zip [1..]

partParser :: Int -> RE (Int, Char) [Part]
partParser row = many (psym (not . isNumber . snd)) *> many (makePart <$> some (psym (isNumber . snd)) <* many (psym (not . isNumber . snd)))
    where
        makePart cs = Part (map snd cs) row $ fst $ head cs

symbolParser :: Int -> RE (Int, Char) [Symbol]
symbolParser row = many (psym (not . isValid . snd)) *> many (makeSymbol <$> (psym (isValid . snd)) <* many (psym (not . isValid . snd)))
    where
        makeSymbol (i, c) = Symbol c row i
        isValid c = (not $ isNumber c) && c /= '.'

adjacency :: Part -> [(Int, Int)]
adjacency p = (,) <$> [(partRow p) - 1 .. (partRow p) + 1] <*> [(partCol p) - 1 .. (partCol p) + (len p)]
    where
        len = length . num

covers :: SymbolSet -> Part -> Bool
covers set = or . map (\(r, c) -> Set.member (Symbol '.' r c) set) . adjacency

updateSymbols :: SymbolSet -> Map.Map Coordinate [Int] -> Part -> Map.Map Coordinate [Int]
updateSymbols set m part = foldl (\cm -> \k -> Map.insertWith (++) k [read $ num part] cm) m $ filter (\(r, c) -> Set.member (Symbol '.' r c) set) $ adjacency part

part1 :: Schematic -> String
part1 (Schematic set parts) = show $ sum $ map (read . num) $ filter (covers set) parts

part2 :: Schematic -> String
part2 (Schematic set parts) = show $ sum $ map (product . snd) $ filter ((==2) . length . snd) $ Map.toList $ foldl (updateSymbols possibleGears) Map.empty parts
    where
        possibleGears :: SymbolSet
        possibleGears = Set.filter ((== '*') . char) set
