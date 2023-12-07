module Llib where

keepValue :: (a -> Bool) -> Maybe a -> Maybe a
keepValue f v = v >>= \x -> if f x then v else Nothing

keepNth :: Int -> [a] -> [a]
keepNth _ [] = []
keepNth n x = head x : keepNth n (drop n x)

replaceItem :: Eq a => a -> a -> [a] -> [a]
replaceItem _ _ [] = []
replaceItem x y (a:as)
    | x == a = y : replaceItem x y as
    | otherwise = a : replaceItem x y as