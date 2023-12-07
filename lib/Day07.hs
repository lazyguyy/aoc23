{-# LANGUAGE OverloadedStrings, TupleSections #-}

module Day07 where

import qualified Data.Text as T

import Text.Regex.Applicative
import Data.Char

data Card = Two | Three | Four | Five | Six | Seven | Eight | Nine | Jack | Queen | King | Ace deriving(Show, Eq, Ord, Enum)
data Hand = Hand {cards :: [Card], bid :: Int} deriving (Show)

cardParser :: RE Char Card
cardParser = ((toEnum . (subtract 2) . digitToInt) <$> (psym isDigit)) <|>
             (sym 'J' *> pure Jack) <|> (sym 'Q' *> pure Queen) <|> (sym 'K' *> pure King) <|> (sym 'A' *> pure Ace)

handParser :: RE Char Hand
handParser = Hand <$> (many cardParser <* sym ' ') <*> (read <$> many (psym isDigit))

