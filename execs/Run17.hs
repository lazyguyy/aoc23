import Day17

import qualified Data.Text.IO as T
import Data.List (intercalate)

main :: IO ()
main = do
    test_input <- T.readFile "inputs/day17-test.in"
    real_input <- T.readFile "inputs/day17.in"
    putStrLn $ "test input\n" <> (intercalate "\n" $ Day17.solve test_input)
    putStrLn $ "real input\n" <> (intercalate "\n" $ Day17.solve real_input)
