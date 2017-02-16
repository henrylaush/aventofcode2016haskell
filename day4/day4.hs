import Text.Parsec
import Text.Parsec.String
import qualified Data.Map.Strict as Map
import qualified Data.List as List
import Data.Either

data RoomCode = RC [String] Int String deriving (Show, Eq)

getId :: RoomCode -> Int
getId (RC _ id _) = id

parseInt :: Parser Int
parseInt = read <$> many1 digit

parseName :: Parser [String]
parseName = many1 (many1 letter <* char '-')

parseCheckSum :: Parser String
parseCheckSum = between (char '[') (char ']') (many1 letter)

parseRoom :: Parser RoomCode
parseRoom = RC <$> parseName <*> parseInt <*> parseCheckSum

checkRoom :: RoomCode -> Bool
checkRoom (RC r _ c) = getCheckSum (concat r) == c

getCheckSum :: String -> String
getCheckSum = map snd . take 5 . List.sort . map (\(k, v) -> (-v, k)) . Map.toAscList . countChar

countChar :: String -> Map.Map Char Int
countChar = foldl (\ac l -> Map.insertWith (+) l 1 ac) Map.empty

-- Part 1
part1 :: [String] -> Int
part1 = sum . map getId . filter checkRoom . rights . map (parse parseRoom "")

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
testCase = ["aaaaa-bbb-z-y-x-123[abxyz]",
            "a-b-c-d-e-f-g-h-987[abcde]",
            "not-a-real-room-404[oarel]",
            "totally-real-room-200[decoy]"]
