{-# LANGUAGE OverloadedStrings, TupleSections #-}

module Day03 where

import qualified Data.Text as T
import qualified Data.Text.IO as T

import Text.Regex.Applicative
import Data.Char
import Data.Set


data Symbol = Symbol {char :: Char, row :: Int, col :: Int} deriving (Show)
type SymbolSet = Set Symbol
data Part = Part {num :: Int, row :: Int, col :: Int} deriving (Show)

data Schematic = Schematic SymbolSet [Part]



readInput :: FilePath -> IO Schematic
readInput = T.lines <$> T.readFile

makeSchematic :: [Text] -> Schematic
makeSchematic = \_ -> Schematic Set.empty []

partParser :: Int -> RE Char Part
partParser row = makePart <$> some (psym isNumber . snd)
    where
        makePart :: [(Char, Char)] -> Part
        makePart cs = Part (read $ map snd cs) row $ fst head cs


part1 :: [Game] -> String
part1 = \_ -> "TODO"

part2 :: [Game] -> String
part2 = \_ -> "TODO"
