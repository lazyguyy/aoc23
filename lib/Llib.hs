module Llib where

import qualified Data.Array as Array
import qualified Data.Text as T

import Data.List (intercalate)
import Data.List.Extra (chunksOf)

keepValue :: (a -> Bool) -> Maybe a -> Maybe a
keepValue f v = v >>= \x -> if f x then v else Nothing

forceMaybe :: String -> Maybe a -> a
forceMaybe message Nothing = error message
forceMaybe _ (Just v) = v

fromJust :: Maybe a -> a
fromJust Nothing = error "fromJust encountered Nothing"
fromJust (Just v) = v

keepNth :: Int -> [a] -> [a]
keepNth _ [] = []
keepNth n x = head x : keepNth n (drop n x)

replaceItem :: Eq a => a -> a -> [a] -> [a]
replaceItem _ _ [] = []
replaceItem x y (a:as)
    | x == a = y : replaceItem x y as
    | otherwise = a : replaceItem x y as

aocBlockInput :: (Char -> a) -> T.Text -> Array.Array (Int, Int) a
aocBlockInput transform text = Array.listArray ((1, 1), (rows, cols)) $ map transform $ concat l
    where
        l = lines $ T.unpack text
        rows = length $ l
        cols = length $ head $ l

aocArrayShow :: (a -> String) -> Array.Array (Int, Int) a -> String
aocArrayShow transform array = intercalate "\n" $ map concat $ chunksOf rows $ map transform $ Array.elems array
    where
        (rows, _) = snd $ Array.bounds array

arrayByRow :: Array.Array (Int, Int) a -> [[a]]
arrayByRow array = chunksOf cols $ Array.elems array
    where
        (_, cols) = snd $ Array.bounds array

findIndex :: (a -> Bool) -> [a] -> Maybe Int
findIndex _ [] = Nothing
findIndex f (a:as) = case f a of
    True -> pure 0
    False -> (+1) <$> findIndex f as