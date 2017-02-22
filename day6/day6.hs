import qualified Data.Map.Strict as Map
import Data.Tuple (swap)
import Data.List (sortOn)
import Data.Ord

-- Part 1
addCharToMap :: Map.Map Char Int -> Char -> Map.Map Char Int
addCharToMap m c = Map.insertWith (+) c 1 m

tally :: [String] -> [[(Char, Int)]]
tally = map Map.toList . foldl (zipWith addCharToMap) (repeat mempty)

part1 :: [String] -> String
part1 = map (fst . head . sortOn (Down . snd)) . tally

-- Part 2
part2 :: [String] -> String
part2 = map (fst . head . sortOn snd) . tally

main :: IO()
main = readInput >>= \content ->
       print (part1 content) >>
       print (part2 content) >>
       return ()

readInput :: IO [String]
readInput = lines <$> readFile "input"

testCase :: [String]
testCase = ["eedadn",
            "drvtee",
            "eandsr",
            "raavrd",
            "atevrs",
            "tsrnev",
            "sdttsa",
            "rasrtv",
            "nssdts",
            "ntnada",
            "svetve",
            "tesnvt",
            "vntsnd",
            "vrdear",
            "dvrsen",
            "enarar"]