import Day12

import qualified Data.Text.IO as T
import Data.List (intercalate)

main :: IO ()
main = do
    test_input <- T.readFile "inputs/day12-test.in"
    real_input <- T.readFile "inputs/day12.in"
    putStrLn $ "test input\n" <> (intercalate "\n" $ Day12.solve test_input)
    putStrLn $ "real input\n" <> (intercalate "\n" $ Day12.solve real_input)
