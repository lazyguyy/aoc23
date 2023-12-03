import Day02

main :: IO ()
main = do
    test_input <- Day03.readInput "inputs/day03-test.in"
    real_input <- Day03.readInput "inputs/day03.in"
    putStrLn $ "Part 1, test input: " <> (show $ fmap Day03.part1 test_input)
    putStrLn $ "Part 1, real input: " <> (show $ fmap Day03.part1 real_input)
    putStrLn $ "Part 2, test input: " <> (show $ fmap Day03.part2 test_input)
    putStrLn $ "Part 2, real input: " <> (show $ fmap Day03.part2 real_input)
