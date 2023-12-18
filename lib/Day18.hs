{-# LANGUAGE TupleSections, OverloadedStrings #-}

module Day18 where

import qualified Data.Text as T

import Data.Maybe(catMaybes)
import Data.Char (isDigit)

import Text.Regex.Applicative

import Numeric (readHex)


data Direction = North | West | South | East deriving (Show, Eq, Enum, Ord)
data Dig = Dig {direction :: Direction, count :: Int, color :: String} deriving (Show, Eq, Ord)

type Coordinate = (Int, Int)
type Polygon = [Coordinate]

move :: Coordinate -> Direction -> Int -> Coordinate
move (row, col) North n = (row - n, col)
move (row, col) South n = (row + n, col)
move (row, col) East n  = (row, col + n)
move (row, col) West n  = (row, col - n)

directionParser :: RE Char Direction
directionParser = (sym 'U' *> pure North) <|>
                  (sym 'R' *> pure East)  <|>
                  (sym 'D' *> pure South) <|>
                  (sym 'L' *> pure West)

digParser :: RE Char Dig
digParser = Dig <$> (directionParser <* sym ' ')
                <*> (read <$> many (psym isDigit) <* sym ' ')
                <*> (string "(#" *> many (psym (/=')')) <* sym ')')

fixDig :: Dig -> Dig
fixDig (Dig _ _ c) = Dig (getDirection $ last c) (fst $ head $ readHex $ init c) ""
    where
        getDirection '0' = West
        getDirection '1' = South
        getDirection '2' = East
        getDirection '3' = North
        getDirection _   = error "Not a valid direction"

parseInput :: T.Text -> [Dig]
parseInput = catMaybes . map ((=~ digParser) . T.unpack) . T.lines

makePolygon :: [Dig] -> Polygon
makePolygon = scanl (\coord dig -> move coord (direction dig) (count dig)) (0, 0)

innerArea :: Polygon -> Int
innerArea corners = abs $ sum $ map rectangleArea $ zip corners $ tail corners
    where
        rectangleArea ((r1, c1), (_, c2)) = r1 * (c1 - c2)

boundaryArea :: Polygon -> Int
boundaryArea corners = sum $ map stripLength $ zip corners $ tail corners
    where
        stripLength ((r1, c1), (r2, c2)) = abs(r2 - r1) + abs(c2 - c1)

totalArea :: Polygon -> Int
totalArea corners = div (boundaryArea corners) 2 + innerArea corners + 1

part1 :: [Dig] -> Int
part1 = totalArea . makePolygon

part2 :: [Dig] -> Int
part2 = part1 . map fixDig

solve :: T.Text -> [String]
solve text = [show $ part1 parsed, show $ part2 parsed]
    where
        parsed = parseInput text
