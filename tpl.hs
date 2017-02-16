

-- Part 1
part1 :: [String] -> Int
part1 = undefined

-- Part 2
part2 :: [String] -> Int
part2 = undefined

main :: IO()
main = readInput >>= \content -> 
       print (part1 content) >>
       print (part2 content) >>
       return ()

readInput :: IO [String]
readInput = lines <$> readFile "input"

testCase :: [Int]
testCase = [1..5] ++ [7..11]