import Day04

main :: IO ()
main = do
    test_input <- Day04.readInput "inputs/day04-test.in"
    real_input <- Day04.readInput "inputs/day04.in"
    putStrLn $ "Part 1, test input: " <> (show $ fmap Day04.part1 test_input)
    putStrLn $ "Part 1, real input: " <> (show $ fmap Day04.part1 real_input)
    putStrLn $ "Part 2, test input: " <> (show $ fmap Day04.part2 test_input)
    putStrLn $ "Part 2, real input: " <> (show $ fmap Day04.part2 real_input)
