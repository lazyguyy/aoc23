{-# LANGUAGE TupleSections #-}

module Day12 where

import qualified Data.Text as T
import qualified Data.Map as Map

import Text.Regex.Applicative

import Data.List (intercalate)
import Data.Char (isDigit)

import Data.Maybe (catMaybes)

data Reconstruction = Rec {segments :: [Int], current :: Int, remaining :: [Int]} deriving (Show, Eq)
instance Ord Reconstruction where
    compare r1 r2 = compare (current r1 : segments r1) $ current r2 : segments r2

isReconstruction :: Reconstruction -> Bool
isReconstruction (Rec _ 0 []) = True
isReconstruction (Rec _ c r)  = [c] == r

data Input = Input {machines :: String, pattern :: [Int]} deriving (Show)

inputParser :: RE Char Input
inputParser = Input <$> many (psym (not . (==' '))) <* sym ' '
                        <*> many (read <$> many (psym isDigit) <* optional (sym ','))

getNext :: Reconstruction -> Char -> [Reconstruction]
getNext (Rec seg cur re@(r:_)) '#' = if cur < r then [Rec seg (cur + 1) re] else []
getNext _ '#'                       = []
getNext (Rec seg cur re) '.' = case (cur, re) of
    (0, _)      -> [Rec seg cur re]
    (_, (x:xs)) -> if x == cur then [Rec (cur : seg) 0 xs] else []
    _           -> []
getNext r _ = getNext r '#' ++ getNext r '.'


singleStep :: Map.Map Reconstruction Int -> Char -> Map.Map Reconstruction Int
singleStep occs char = foldl (\m (elmt, count) -> foldl (insertIntoMap count) m $ getNext elmt char) Map.empty $ Map.assocs occs
    where
        insertIntoMap :: Int -> Map.Map Reconstruction Int -> Reconstruction -> Map.Map Reconstruction Int
        insertIntoMap count m r = Map.insertWith (+) r count m

constructPossibilites :: Input -> Map.Map Reconstruction Int
constructPossibilites input = foldl singleStep (Map.fromList [(Rec [] 0 $ pattern input, 1)]) $ machines input

part1 :: [Input] -> String
part1 inputs = show $ sum $ map (sum . map snd . filter (isReconstruction . fst) . Map.toList . constructPossibilites) inputs

unfold :: Input -> Input
unfold (Input m p) = Input (intercalate "?" $ replicate 5 m) $ concat $ replicate 5 p

solve :: T.Text -> [String]
solve text = [part1 parsed, part1 $ map (unfold) parsed]
    where
        parsed = catMaybes $ map (=~inputParser) $ lines $ T.unpack text