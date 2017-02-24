import Text.Parsec
import Text.Parsec.String
import Data.Either (rights)
import qualified Data.Set as Set
import Data.List (intercalate)

type Board = Set.Set (Int, Int)
type BoardSize = (Int, Int)
data CMD = R Int Int | RC Int Int | RR Int Int deriving (Show, Eq)

parseInt :: Parser Int
parseInt = read <$> many digit

parseCMD :: Parser CMD
parseCMD = choice $ map try [parseRect, parseRC, parseRR]

parseRect :: Parser CMD
parseRect = R <$> (string "rect " *> parseInt) <*> (char 'x' *> parseInt)

parseRC :: Parser CMD
parseRC = RC <$> (string "rotate column x=" *> parseInt) <*> (string " by " *> parseInt)

parseRR :: Parser CMD
parseRR = RR <$> (string "rotate row y=" *> parseInt) <*> (string " by " *> parseInt)

rotateColumn :: BoardSize -> Int -> Int -> (Int, Int) -> (Int, Int)
rotateColumn (rs, _) cn s p@(r, c) 
    | cn == c = ( (r + s) `mod` rs , c)
    | otherwise = p

rotateRow :: BoardSize -> Int -> Int -> (Int, Int) -> (Int, Int)
rotateRow (_, cs) rn s p@(r,c)
    | rn == r = (r, (c + s) `mod` cs)
    | otherwise = p

runCMD :: BoardSize -> Board -> CMD -> Board
runCMD (rs, cs) b (R c r) = Set.union b $ Set.fromList $ map (,) [0..(min r rs - 1)]  <*> [0..(min cs c - 1)]
runCMD bs b (RC cn s) = Set.map (rotateColumn bs cn s) b
runCMD bs b (RR rn s) = Set.map (rotateRow bs rn s) b

-- Part 1
makeBoard :: [String] -> Board
makeBoard = foldl (runCMD (6, 50)) mempty . rights . map (parse parseCMD "")

part1 :: [String] -> Int
part1 = Set.size . makeBoard

-- Part 2
symbolAtPos :: Board -> (Int, Int) -> Char
symbolAtPos b p | Set.member p b = '-' | otherwise = ' '

part2 :: [String] -> String
part2 cmd = intercalate "\n" [[ symbolAtPos b (i, j) | j <- [0..(50-1)]] | i <- [0..(6-1)]]
          where b = makeBoard cmd

main :: IO()
main = readInput >>= \content -> 
       print (part1 content) >>
       putStr (part2 content) >>
       return ()

readInput :: IO [String]
readInput = lines <$> readFile "input"

testBoardSize :: BoardSize
testBoardSize = (3, 7)

testCase :: [String]
testCase = ["rect 3x2", "rotate column x=1 by 1", "rotate row y=0 by 4", "rotate column x=1 by 1"]