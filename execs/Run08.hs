import Day08

import qualified Data.Text.IO as T
import Data.List (intercalate)

main :: IO ()
main = do
    test_input <- T.readFile "inputs/day08-test.in"
    test2_input <- T.readFile "inputs/day08-test2.in"
    real_input <- T.readFile "inputs/day08.in"
    putStrLn $ "test input: " <> (intercalate "\n" $ Day08.solve test_input)
    putStrLn $ "test input: " <> (intercalate "\n" $ Day08.solve test2_input)
    putStrLn $ "real input: " <> (intercalate "\n" $ Day08.solve real_input)
