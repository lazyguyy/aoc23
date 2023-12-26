import Day19

import qualified Data.Text.IO as T
import Data.List (intercalate)

main :: IO ()
main = do
    test_input <- T.readFile "inputs/day19-test.in"
    real_input <- T.readFile "inputs/day19.in"
    putStrLn $ "test input\n" <> (intercalate "\n" $ Day19.solve test_input)
    putStrLn $ "real input\n" <> (intercalate "\n" $ Day19.solve real_input)
