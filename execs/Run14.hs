import Day14

import qualified Data.Text.IO as T
import Data.List (intercalate)

main :: IO ()
main = do
    test_input <- T.readFile "inputs/day14-test.in"
    real_input <- T.readFile "inputs/day14.in"
    putStrLn $ "test input\n" <> (intercalate "\n\n" $ Day14.solve test_input)
    putStrLn $ "real input\n" <> (intercalate "\n" $ Day14.solve real_input)
