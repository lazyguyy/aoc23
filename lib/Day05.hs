{-# LANGUAGE OverloadedStrings, TupleSections #-}

module Day05 where

import qualified Data.Text as T
import qualified Data.Map as Map

import Text.Regex.Applicative
import Data.Char
import Data.Maybe (fromMaybe)

import Llib

data Transformation = Transformation {dest :: Int, src :: Int, range :: Int} deriving Show
data Mapping = Mapping {name :: String, mapping :: Map.Map Int Transformation} deriving (Show)
data Input = Input [Int] [Mapping] deriving (Show)
-- to is exclusive
data Range = Range {from :: Int, to :: Int} deriving (Show)

upTo :: Transformation -> Int
upTo (Transformation _ s r) = s + r

emptyParser :: RE Char Char
emptyParser = sym ' ' <|> sym '\n'

-- short for transformationParser
tParser :: RE Char Transformation
tParser = Transformation <$>
          (read <$> (many (psym isNumber) <* emptyParser)) <*>
          (read <$> (many (psym isNumber) <* emptyParser)) <*>
          (read <$> (many (psym isNumber) <* emptyParser))

mapParser :: RE Char Mapping
mapParser = makeMapping <$>
            (many (psym (/=':')) <* string ":\n") <*>
            (many tParser) <* optional emptyParser

inputParser :: RE Char Input
inputParser = Input <$>
              (string "seeds: " *> (many (read <$> many (psym isNumber) <* emptyParser) <* emptyParser)) <*>
              many mapParser

makeMapping :: String -> [Transformation] -> Mapping
makeMapping s ts = Mapping s $ foldl insertIntoMap Map.empty ts
    where
        insertIntoMap m t = Map.insert (src t) t m

listMapping :: [Int] -> Mapping -> [Int]
listMapping vs (Mapping _ m) = map transform vs
    where
        transform v = fromMaybe v $ itemTransform v <$> (snd <$> Map.lookupLE v m)

itemTransform :: Int -> Transformation -> Int
itemTransform v t
    | (src t) + (range t) > v = (dest t) + v - (src t)
    | otherwise               = v

part1 :: Input -> Int
part1 (Input seeds mappings) = minimum $ foldl listMapping seeds mappings

rangeTransform :: Range -> Transformation -> Range
rangeTransform (Range f t) (Transformation d s r) = Range (f + shift) $ (min t $ s + r) + shift
    where
        shift = d - s

rangeMapping :: Range -> Mapping -> [Range]
rangeMapping r@(Range f t) mp@(Mapping _ m)
    | f >= t = []
    | otherwise = rangeTransform r actual : rangeMapping (Range (upTo actual) t) mp
    where
        lower = keepValue (\trf -> src trf + range trf > f) $ snd <$> Map.lookupLE f m
        higher = Transformation f f $ (fromMaybe t $ (src . snd) <$> Map.lookupGT f m) - f
        actual = fromMaybe higher lower

makeRanges :: [Int] -> [Range]
makeRanges ns = map makeRange $ keepNth 2 $ zip ns $ tail ns
    where
        makeRange (x, y) = Range x $ x + y

part2 :: Input -> Int
part2 (Input seeds mappings) = minimum $ map from $ foldl (\r m -> concat $ map (flip rangeMapping m) r) (makeRanges seeds) mappings

solve :: T.Text -> [String]
solve text = [show $ fmap part1 parsed, show $ fmap part2 parsed]
    where
        parsed = (T.unpack text) =~ inputParser
