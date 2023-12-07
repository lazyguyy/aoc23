import Day06

import qualified Data.Text.IO as T
import Data.List (intercalate)

main :: IO ()
main = do
    test_input <- T.readFile "inputs/day06-test.in"
    real_input <- T.readFile "inputs/day06.in"
    putStrLn $ "test input: " <> (intercalate "\n" $ Day06.solve test_input)
    putStrLn $ "real input: " <> (intercalate "\n" $ Day06.solve real_input)
