import Text.Parsec
import Text.Parsec.String
import Data.Either

data Triangle = T Int Int Int deriving (Show, Eq)

-- Part 1
parseInt :: Parser Int
parseInt = read <$> (spaces *> many1 digit)

parseTriangle :: Parser Triangle
parseTriangle = T <$> parseInt <*> parseInt <*> parseInt

isValidTriangle :: Triangle -> Bool
isValidTriangle (T x y z) = and [(x + y > z), (y + z) > x, (z + x) > y]

part1 :: [String] -> Int
part1 = length . filter isValidTriangle . rights . map (parse parseTriangle "")

-- Part 2
columnWise :: [[Int]] -> [Triangle]
columnWise [] = []
columnWise (x:[]) = []
columnWise (x:y:[]) = []
columnWise (x:y:z:xs) = zipWith3 T x y z ++ columnWise xs

part2 :: [String] -> Int
part2 = length . filter isValidTriangle . columnWise . rights . map (parse (many1 parseInt) "")

main :: IO()
main = readInput >>= \content ->
       print (part1 content) >>
       print (part2 content) >>
       return ()

readInput :: IO [String]
readInput = lines <$> readFile "input"

testCase :: String
testCase = "  775  785  361\n  622  375  125\n  297  839  375"