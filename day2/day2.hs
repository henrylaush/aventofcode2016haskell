-- 1 2 3
-- 4 5 6
-- 7 8 9

fsm :: Char -> Char -> Char
fsm '1' 'U' = '1'
fsm '1' 'R' = '2'
fsm '1' 'D' = '4'
fsm '1' 'L' = '1'
fsm '2' 'U' = '2'
fsm '2' 'R' = '3'
fsm '2' 'D' = '5'
fsm '2' 'L' = '1'
fsm '3' 'U' = '3'
fsm '3' 'R' = '3'
fsm '3' 'D' = '6'
fsm '3' 'L' = '2'
fsm '4' 'U' = '1'
fsm '4' 'R' = '5'
fsm '4' 'D' = '7'
fsm '4' 'L' = '4'
fsm '5' 'U' = '2'
fsm '5' 'R' = '6'
fsm '5' 'D' = '8'
fsm '5' 'L' = '4'
fsm '6' 'U' = '3'
fsm '6' 'R' = '6'
fsm '6' 'D' = '9'
fsm '6' 'L' = '5'
fsm '7' 'U' = '4'
fsm '7' 'R' = '8'
fsm '7' 'D' = '7'
fsm '7' 'L' = '7'
fsm '8' 'U' = '5'
fsm '8' 'R' = '9'
fsm '8' 'D' = '8'
fsm '8' 'L' = '7'
fsm '9' 'U' = '6'
fsm '9' 'R' = '9'
fsm '9' 'D' = '9'
fsm '9' 'L' = '8'
fsm _ _ = error "Not in set"

-- Part 1
part1 :: [String] -> String
part1 = drop 1 . scanl (foldl fsm) '5'

--     1
--   2 3 4
-- 5 6 7 8 9
--   A B C
--     D
fsm2 :: Char -> Char -> Char
fsm2 '1' 'U' = '1'
fsm2 '1' 'R' = '1'
fsm2 '1' 'D' = '3'
fsm2 '1' 'L' = '1'
fsm2 '2' 'U' = '2'
fsm2 '2' 'R' = '3'
fsm2 '2' 'D' = '6'
fsm2 '2' 'L' = '2'
fsm2 '3' 'U' = '1'
fsm2 '3' 'R' = '4'
fsm2 '3' 'D' = '7'
fsm2 '3' 'L' = '2'
fsm2 '4' 'U' = '4'
fsm2 '4' 'R' = '4'
fsm2 '4' 'D' = '8'
fsm2 '4' 'L' = '3'
fsm2 '5' 'U' = '5'
fsm2 '5' 'R' = '6'
fsm2 '5' 'D' = '5'
fsm2 '5' 'L' = '5'
fsm2 '6' 'U' = '2'
fsm2 '6' 'R' = '7'
fsm2 '6' 'D' = 'A'
fsm2 '6' 'L' = '5'
fsm2 '7' 'U' = '3'
fsm2 '7' 'R' = '8'
fsm2 '7' 'D' = 'B'
fsm2 '7' 'L' = '6'
fsm2 '8' 'U' = '4'
fsm2 '8' 'R' = '9'
fsm2 '8' 'D' = 'C'
fsm2 '8' 'L' = '7'
fsm2 '9' 'U' = '9'
fsm2 '9' 'R' = '9'
fsm2 '9' 'D' = '9'
fsm2 '9' 'L' = '8'
fsm2 'A' 'U' = '6'
fsm2 'A' 'R' = 'B'
fsm2 'A' 'D' = 'A'
fsm2 'A' 'L' = 'A'
fsm2 'B' 'U' = '7'
fsm2 'B' 'R' = 'C'
fsm2 'B' 'D' = 'D'
fsm2 'B' 'L' = 'A'
fsm2 'C' 'U' = '8'
fsm2 'C' 'R' = 'C'
fsm2 'C' 'D' = 'C'
fsm2 'C' 'L' = 'B'
fsm2 'D' 'U' = 'B'
fsm2 'D' 'R' = 'D'
fsm2 'D' 'D' = 'D'
fsm2 'D' 'L' = 'D'
fsm2 _ _ = error "Not in set"

-- Part 2
part2 :: [String] -> String
part2 = drop 1 . scanl (foldl fsm2) '5'

main :: IO()
main = readInput >>= \content ->
       print (part1 content) >>
       print (part2 content) >>
       return ()

readInput :: IO [String]
readInput = lines <$> readFile "input"

testCase :: [String]
testCase = ["ULL","RRDDD","LURDL","UUUUD"]