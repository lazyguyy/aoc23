import Day05

import qualified Data.Text.IO as T

import Data.List (intercalate)

main :: IO ()
main = do
    test_input <- T.readFile "inputs/day05-test.in"
    real_input <- T.readFile "inputs/day05.in"
    putStrLn $ "test input: " ++ (intercalate "\n" $ Day05.solve test_input)
    putStrLn $ "real input: " ++ (intercalate "\n" $ Day05.solve real_input)
