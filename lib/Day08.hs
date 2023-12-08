{-# LANGUAGE OverloadedStrings, TupleSections #-}

module Day08 where

import qualified Data.Text as T
import qualified Data.Map as Map

import Text.Regex.Applicative
import Data.Maybe (fromMaybe)
import Data.Bool.HT (ifThenElse)

data Instruction = L | R deriving (Show)

data Node = Node {name :: Name, left :: Name, right :: Name} deriving (Show)
takePath :: Instruction -> Node -> Name
takePath L (Node _ l _) = l
takePath R (Node _ _ r) = r

type Name = String
type Network = Map.Map String Node

parseInstructions :: RE Char [Instruction]
parseInstructions = many (sym 'L' *> pure L <|> sym 'R' *> pure R)

parseMap :: RE Char Network
parseMap = makeMap <$> many (Node <$> parseString <*> parseString <*> parseString <* (optional $ psym ignoreLetter))
    where
        makeMap ns = Map.fromList $ zip (map name ns) ns

ignoreLetter :: Char -> Bool
ignoreLetter = flip elem (" ()=,\n" :: String)

parseString :: RE Char String
parseString = many (psym $ not . ignoreLetter) <* many (psym ignoreLetter)

parseInput :: RE Char ([Instruction], Network)
parseInput = (,) <$> parseInstructions <* (many $ psym ignoreLetter) <*> parseMap

walk :: Network -> (Name -> Bool) -> Name -> [Instruction] -> [Name]
walk _ _ _ [] = []
walk m isGoal cur (i:is) = case followMap m cur i of
    Nothing   -> []
    Just next -> ifThenElse (isGoal next) [next] $ next : (walk m isGoal next is)

followMap :: Network -> Name -> Instruction -> Maybe Name
followMap m n i = takePath i <$> Map.lookup n m


part1 :: Network -> [Instruction] -> String
part1 m = show . length . walk m (== "ZZZ") "AAA" . cycle

part2 :: Network -> [Instruction] -> String
part2 m is = show $ foldl lcm 1 $ map (\s -> length . walk m ((=='Z') . last) s $ cycle is) $ filter ((=='A') . last) $ Map.keys m

solve :: T.Text -> [String]
solve input = [part1 mp ins, part2 mp ins]
    where
        (ins, mp) = fromMaybe ([], Map.empty) $ (T.unpack input) =~ parseInput
