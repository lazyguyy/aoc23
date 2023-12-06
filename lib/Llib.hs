module Llib where

keepValue :: (a -> Bool) -> Maybe a -> Maybe a
keepValue f v = v >>= \x -> if f x then v else Nothing

keepNth :: Int -> [a] -> [a]
keepNth _ [] = []
keepNth n x = head x : keepNth n (drop n x)