{-# LANGUAGE OverloadedStrings, TupleSections #-}

module Day07 where

import qualified Data.Text as T

import Text.Regex.Applicative
import Data.Char
import Data.Maybe (catMaybes)
import Data.List (group, sort, sortBy)
import Llib (replaceItem)

data Card = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace deriving(Show, Eq, Ord, Enum, Bounded)
data MagicCard = MC Card deriving (Show)
instance Ord MagicCard where
    compare (MC Jack) (MC Jack) = EQ
    compare (MC Jack) _ = LT
    compare _ (MC Jack) = GT
    compare (MC c1) (MC c2) = compare c1 c2

instance Eq MagicCard where
    (MC c1) == (MC c2) = c1 == c2

data Hand = Hand {cards :: [Card], bid :: Int} deriving (Show)

cardParser :: RE Char Card
cardParser = ((toEnum . (subtract 2) . digitToInt) <$> (psym isDigit)) <|>
            (sym 'T' *> pure Ten) <|> (sym 'J' *> pure Jack) <|> (sym 'Q' *> pure Queen) <|> (sym 'K' *> pure King) <|> (sym 'A' *> pure Ace)

handParser :: RE Char Hand
handParser = Hand <$> (many cardParser <* sym ' ') <*> (read <$> many (psym isDigit))

rankValue :: Hand -> [Int]
rankValue = reverse . sort . map length . group . sort . cards

magicRankValue :: Card -> Hand -> [Int]
magicRankValue c = reverse . sort . map length . group . sort . replaceItem Jack c . cards

maxMagicRank :: Hand -> [Int]
maxMagicRank hand = maximum . map (flip magicRankValue hand) $ [(minBound :: Card)..(maxBound :: Card)]

part1 :: [Hand] -> String
part1 = show . sum . zipWith (*) [1..] . map bid . sortBy comp
    where
        comp a b
            | rankValue a == rankValue b = compare (cards a) $ cards b
            | otherwise = compare (rankValue a) $ rankValue b

part2 :: [Hand] -> String
part2 = show . sum . zipWith (*) [1..] . map bid . sortBy comp
    where
        comp a b
            | maxMagicRank a == maxMagicRank b = compare (map MC $ cards a) $ map MC $ cards b
            | otherwise = compare (maxMagicRank a) $ maxMagicRank b

solve :: T.Text -> [String]
solve input = [part1 parsed, part2 parsed]
    where
        parsed = catMaybes $ map ((=~ handParser) . T.unpack) $ T.lines input