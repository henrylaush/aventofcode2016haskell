import Text.Parsec
import Text.Parsec.String

data Coords = C Int Int deriving (Eq, Show)
data Direction = North | East | South | West deriving (Eq, Show)
data Action = R Int | L Int deriving (Eq, Show)
data Santa = S Coords Direction deriving (Eq, Show)

getCoords :: Santa -> Coords
getCoords (S c _) = c

initial :: Santa
initial = S (C 0 0) North

parseR :: Parser (Int -> Action)
parseR = R <$ char 'R'

parseL :: Parser (Int -> Action)
parseL = L <$ char 'L'

parseDigits :: Parser Int
parseDigits = read <$> many1 digit

parseAction :: Parser Action
parseAction = (parseR <|> parseL) <*> parseDigits

parseInput :: Parser [Action]
parseInput = parseAction `sepBy` string ", "

step :: Santa -> Action -> Santa
step (S (C x y) d) a = S (C (x + dis * fst p) (y + dis * snd p)) nd
                     where
                        nd = newDirection d a
                        p = pace nd
                        dis = distance a

runSteps :: [Action] -> Int
runSteps = blocksAway . getCoords . foldl step initial

distance :: Action -> Int
distance (R i) = i
distance (L i) = i

blocksAway :: Coords -> Int
blocksAway (C x y) = abs x + abs y

pace :: Direction -> (Int, Int)
pace North = (0, 1)
pace East = (1, 0)
pace South = (0, -1)
pace West = (-1, 0)

newDirection :: Direction -> Action -> Direction
newDirection North (R _) = East
newDirection North (L _) = West
newDirection East (R _) = South
newDirection East (L _) = North
newDirection South (R _) = West
newDirection South (L _) = East
newDirection West (R _) = North
newDirection West (L _) = South

-- Part 1
part1 :: String -> Int
part1 = either (const 0) runSteps . parse parseInput ""

path :: [Action] -> [Coords]
path = map getCoords . scanl step initial

detailedPath :: [Coords] -> [Coords]
detailedPath p = concatMap stepsBetween (zip p (drop 1 p)) ++ [last p]

stepsBetween :: (Coords, Coords) -> [Coords]
stepsBetween (C fx fy, C tx ty) = take d $ zipWith C (iterate (((tx - fx) `div` d) + ) fx) (iterate (((ty - fy) `div` d) + ) fy)
                                    where d = abs (tx - fx + ty - fy)

firstRepeat :: [Coords] -> Coords
firstRepeat ls = head $ dropWhile (\x -> countList (x == ) ls < 2) ls

countList :: (a -> Bool) -> [a] -> Int
countList pred = length . filter pred

runP2 :: [Action] -> Int
runP2 =  blocksAway . firstRepeat . detailedPath . path


-- Part 2
part2 :: String -> Int
part2 = either (const 0) runP2 . parse parseInput ""

main :: IO()
main = readInput >>= \content ->
       print (part1 content) >>
       print (part2 content) >>
       return ()

readInput :: IO String
readInput = readFile "input"

testCase1 = "R2, L3" -- 5
testCase2 = "R2, R2, R2" -- 2
testCase3 = "R5, L5, R5, R3" -- 12
