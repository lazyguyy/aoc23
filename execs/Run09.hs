import Day09

import qualified Data.Text.IO as T
import Data.List (intercalate)

main :: IO ()
main = do
    test_input <- T.readFile "inputs/day09-test.in"
    real_input <- T.readFile "inputs/day09.in"
    putStrLn $ "test input: " <> (intercalate "\n" $ Day09.solve test_input)
    putStrLn $ "real input: " <> (intercalate "\n" $ Day09.solve real_input)
