import Day11

import qualified Data.Text.IO as T
import Data.List (intercalate)

main :: IO ()
main = do
    test_input <- T.readFile "inputs/day11-test.in"
    real_input <- T.readFile "inputs/day11.in"
    putStrLn $ "test input\n" <> (intercalate "\n" $ Day11.solve test_input)
    putStrLn $ "real input\n" <> (intercalate "\n" $ Day11.solve real_input)
