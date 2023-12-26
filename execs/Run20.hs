import Day20

import qualified Data.Text.IO as T
import Data.List (intercalate)

main :: IO ()
main = do
    test_input1 <- T.readFile "inputs/day20-test1.in"
    test_input2 <- T.readFile "inputs/day20-test2.in"
    real_input <- T.readFile "inputs/day20.in"
    putStrLn $ "test input 1\n" <> (intercalate "\n" $ Day20.solve test_input1)
    putStrLn $ "test input 2\n" <> (intercalate "\n" $ Day20.solve test_input2)
    putStrLn $ "real input\n" <> (intercalate "\n" $ Day20.solve real_input)
