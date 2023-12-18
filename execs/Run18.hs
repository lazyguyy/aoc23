import Day18

import qualified Data.Text.IO as T
import Data.List (intercalate)

main :: IO ()
main = do
    test_input <- T.readFile "inputs/day18-test.in"
    real_input <- T.readFile "inputs/day18.in"
    putStrLn $ "test input\n" <> (intercalate "\n" $ Day18.solve test_input)
    putStrLn $ "real input\n" <> (intercalate "\n" $ Day18.solve real_input)
