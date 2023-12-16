import Day13

import qualified Data.Text.IO as T
import Data.List (intercalate)

main :: IO ()
main = do
    test_input <- T.readFile "inputs/day13-test.in"
    real_input <- T.readFile "inputs/day13.in"
    putStrLn $ "test input\n" <> (intercalate "\n" $ Day13.solve test_input)
    putStrLn $ "real input\n" <> (intercalate "\n" $ Day13.solve real_input)
