import Day16

import qualified Data.Text.IO as T
import Data.List (intercalate)

main :: IO ()
main = do
    test_input <- T.readFile "inputs/day16-test.in"
    real_input <- T.readFile "inputs/day16.in"
    putStrLn $ "test input\n" <> (intercalate "\n" $ Day16.solve test_input)
    putStrLn $ "real input\n" <> (intercalate "\n" $ Day16.solve real_input)
