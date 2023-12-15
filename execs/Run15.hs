import Day15

import qualified Data.Text.IO as T
import Data.List (intercalate)

main :: IO ()
main = do
    test_input <- T.readFile "inputs/day15-test.in"
    real_input <- T.readFile "inputs/day15.in"
    putStrLn $ "test input\n" <> (intercalate "\n" $ Day15.solve test_input)
    putStrLn $ "real input\n" <> (intercalate "\n" $ Day15.solve real_input)
