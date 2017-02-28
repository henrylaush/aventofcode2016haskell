import Text.Parsec
import Text.Parsec.String
import Data.Either (rights)
import Data.List (sum)

data Part = R Char | M Int Int deriving (Show, Eq)
data FState = A Int | D Int Int deriving (Show, Eq)

partLength :: Part -> Int
partLength (R _) = 1
partLength (M a b) = 3 + length (show a) + length (show b)

parseInt :: Parser Int
parseInt = read <$> many digit

parsePart :: Parser [Part]
parsePart = many (R <$> try upper <|> try parseMarker)

parseMarker :: Parser Part
parseMarker = between (char '(') (char ')') (M <$> parseInt <* char 'x' <*> parseInt)

expand :: FState -> Part -> FState
expand (A t) (R _) = A (t + 1)
expand (A t) (M a b) = D (t + a * b) a
expand (D t td) (R _) | td == 1 = A t | otherwise = D t (td - 1)
expand (D t td) m@(M _ _) 
    | ml < td = D t (td - ml)
    | otherwise = A (t + ml - td)
    where ml = partLength m

getLength :: FState -> Int
getLength (A a) = a
getLength D{} = error "Still dropping"

-- Part 1
part1 :: [String] -> Int
part1 = sum . map (getLength . foldl expand (A 0)) . rights . map (parse parsePart "")

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

testCase :: [String]
testCase = ["ADVENT", -- 6
            "A(1x5)BC", -- 7 
            "(3x3)XYZ", -- 9
            "A(2x2)BCD(2x2)EFG", -- 11
            "(6x1)(1x3)A", -- 6
            "X(8x2)(3x3)ABCY"] -- 18

testCase2 :: [String]
testCase2 = ["(3x3)XYZ", -- 9
             "X(8x2)(3x3)ABCY", -- 20 
             "(27x12)(20x12)(13x14)(7x10)(1x12)A", -- 241920
             "(25x3)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN"] -- 445