{-# LANGUAGE OverloadedStrings, TupleSections #-}

module Day02 (readInput, part1, part2) where

import qualified Data.Text as T
import qualified Data.Text.IO as T

import Text.Regex.Applicative
import Data.Char

data Game = Game Int [CubeSet] deriving (Show)
gameID :: Game -> Int
gameID (Game gid _) = gid

cubeSets :: Game -> [CubeSet]
cubeSets (Game _ cs) = cs

type CubeSet = [Cubes]
data Cubes = Cubes Color Int deriving (Show)

color :: Cubes -> Color
color (Cubes c _ ) = c

count :: Cubes -> Int
count (Cubes _ c) = c

data Color = Red | Green | Blue deriving (Enum, Show, Eq)

part1CubeSet :: CubeSet
part1CubeSet = [Cubes Red 12, Cubes Green 13, Cubes Blue 14]

readInput :: FilePath -> IO (Maybe [Game])
readInput = fmap (sequence . fmap ((=~ gameParser) . T.unpack) . T.lines) . T.readFile

colorParser :: RE Char Color
colorParser = string "red" *> pure Red <|> string "green" *> pure Green <|> string "blue" *> pure Blue

intParser :: RE Char Int
intParser = read <$> some (psym isNumber)

cubeParser :: RE Char Cubes
cubeParser = (flip Cubes) <$> intParser <* sym ' ' <*> colorParser

cubeSetParser :: RE Char CubeSet
cubeSetParser = many (cubeParser <* optional (string ", "))

gameParser :: RE Char Game
gameParser = Game <$> (string "Game " *> intParser) <*> (string ": " *> many (cubeSetParser <* optional (string "; ")))

isValid :: CubeSet -> Game -> Bool
isValid reference (Game _ cs) = and $ map (isContained reference) cs

isContained :: CubeSet -> CubeSet -> Bool
isContained reference set = and $ map (flip hasCubes reference) set
    where
        hasCubes :: Cubes -> CubeSet -> Bool
        hasCubes cubes cubeset = or $ isLess <$> cubeset <*> pure cubes

getMaxSet :: [CubeSet] -> CubeSet
getMaxSet = foldl maxSet [Cubes Red 0, Cubes Green 0, Cubes Blue 0]
    where
        maxCubes set1 set2 c = Cubes c $ maximum $ map count $ filter ((==c) . color) (set1 ++ set2)
        maxSet set1 set2 = map (maxCubes set1 set2) [Red, Green, Blue]

isLess :: Cubes -> Cubes -> Bool
isLess (Cubes c1 n1) (Cubes c2 n2) = c1 == c2 && n1 >= n2

part1 :: [Game] -> String
part1 = show . sum . map gameID . filter (isValid part1CubeSet)

part2 :: [Game] -> String
-- part2 = \_ -> "Todo"
part2 = show . sum . map (product . map count . getMaxSet . cubeSets)