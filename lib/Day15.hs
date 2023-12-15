{-# LANGUAGE TupleSections, OverloadedStrings #-}

module Day15 where

import qualified Data.Text as T
import qualified Data.Map as Map

import Text.Regex.Applicative
import Data.Char (isDigit, isLetter)


data Operation = Update String Int | Dash String
instance Show Operation where
    show (Update s i) = s ++ "=" ++ show i
    show (Dash s) = s ++ "-"

name :: Operation -> String
name (Update s _) = s
name (Dash s) = s

type Lens = (String, Int)
type Box = [Lens]
type Boxes = Map.Map Int Box

hash :: String -> Int -> Int
hash [] value = value
hash (c:cs) value = hash cs $ mod ((value + fromEnum c) * 17) 256

parseOp :: RE Char Operation
parseOp = ((Update <$> many (psym isLetter)) <* sym '=' <*> (read <$> many (psym isDigit))) <|>
          (Dash <$> many (psym isLetter) <* sym '-')

parseInput :: RE Char [Operation]
parseInput = many (parseOp <* optional (sym ','))

part1 :: [Operation] -> Int
part1 = sum . map (flip hash 0 . show)

dash :: Box -> String -> Box
dash [] _ = []
dash (l:ls) label = if fst l == label then ls else l : dash ls label

update :: Box -> String -> Int -> Box
update [] label value = [(label, value)]
update (l:ls) label value = if fst l == label then (label, value) : ls else l : update ls label value

execute :: Boxes -> Operation -> Boxes
execute boxes op = case op of
    (Update s val) -> Map.insert index (update box s val) boxes
    (Dash s) -> Map.insert index (dash box s) boxes
    where
        index = hash (name op) 0
        box = Map.findWithDefault [] index boxes

focusingPower :: Boxes -> Int
focusingPower boxes = sum $ zipWith (*) [1..] $ map boxPower $ map (\i -> Map.findWithDefault [] i boxes) [0..255]
    where
        boxPower box = sum $ zipWith (*) [1..] $ map snd box

part2 :: [Operation] -> Int
part2 = focusingPower . foldl (execute) Map.empty

solve :: T.Text -> [String]
solve text = [show $ fmap part1 parsed, show $ fmap part2 parsed]
    where
        parsed = (T.unpack $ T.strip text) =~ parseInput
