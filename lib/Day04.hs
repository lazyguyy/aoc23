{-# LANGUAGE OverloadedStrings, TupleSections #-}

module Day04 (readInput, part1, part2) where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Set as Set
import qualified Data.Map as Map

import Text.Regex.Applicative
import Data.Char

type NumberSet = Set.Set Int
data Card = Card {idx :: Int, winning :: NumberSet, actual :: NumberSet} deriving (Show)

readInput :: FilePath -> IO (Maybe [Card])
readInput = fmap (sequence . fmap ((=~ cardParser) . T.unpack) . T.lines) . T.readFile

numberParser :: RE Char Int
numberParser = read <$> many (psym isNumber) <* many (sym ' ')

cardParser :: RE Char Card
cardParser = makeCard <$> (string "Card" *> many (sym ' ') *> numberParser) <*> (sym ':' *> many (sym ' ') *> many numberParser) <*> (sym '|' *> many (sym ' ') *> many numberParser)

makeCard :: Int -> [Int] -> [Int] -> Card
makeCard i w a = Card i (Set.fromList w) (Set.fromList a)


part1 :: [Card] -> String
part1  = show . sum .  map cardValue
    where
        cardValue (Card _ w a) = div (2 ^ (Set.size $ Set.intersection w a)) 2


part2 :: [Card] -> String
part2 cards = show $ Map.foldr (+) 0 $ foldl (\m -> \c -> updateMap m c) (Map.fromList $ zip [1..(length cards)] $ repeat 1) cards
    where
        updateMap :: Map.Map Int Int -> Card -> Map.Map Int Int
        updateMap mp (Card i w a) = foldl (\m -> \k -> Map.insertWith (+) k (Map.findWithDefault 1 i mp) m) mp [i + 1..i + (Set.size $ Set.intersection w a)]
