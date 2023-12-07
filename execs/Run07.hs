import Day07

import qualified Data.Text.IO as T
import Data.List (intercalate)

main :: IO ()
main = do
    test_input <- T.readFile "inputs/day07-test.in"
    real_input <- T.readFile "inputs/day07.in"
    putStrLn $ "test input: " <> (intercalate "\n" $ Day07.solve test_input)
    putStrLn $ "real input: " <> (intercalate "\n" $ Day07.solve real_input)
