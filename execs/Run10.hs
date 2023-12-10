import Day10

import qualified Data.Text.IO as T
import Data.List (intercalate)

main :: IO ()
main = do
    test_input <- T.readFile "inputs/day10-test.in"
    test_input2 <- T.readFile "inputs/day10-test2.in"
    real_input <- T.readFile "inputs/day10.in"
    putStrLn $ "test input\n" <> (intercalate "\n" $ Day10.solve test_input)
    putStrLn $ "test input 2\n" <> (intercalate "\n" $ Day10.solve test_input2)
    putStrLn $ "real input\n" <> (intercalate "\n" $ Day10.solve real_input)
