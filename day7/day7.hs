import Text.Parsec
import Text.Parsec.String
import Data.Either (rights)
import qualified Data.Set as Set

type Address = ([String], [String])

both :: (a -> b) -> (a, a) -> (b, b)
both f (c, d) = (f c, f d)

parseLowerString :: Parser String
parseLowerString = many lower

parseNormal :: Parser Address
parseNormal = flip (,) [] . pure <$> parseLowerString

parseHyperNetSeq :: Parser Address
parseHyperNetSeq = (,) [] . pure <$> between (char '[') (char ']') parseLowerString

parseRepeating :: Parser Address
parseRepeating = fmap mconcat $ many $ mappend <$> parseHyperNetSeq <*> parseNormal

parseAddress :: Parser Address
parseAddress = mappend <$> parseNormal <*> parseRepeating

isxyyx :: String -> Bool
isxyyx (a:b:c:d:r) = (a /= b && b == c && a == d) || isxyyx ([b,c,d] ++ r)
isxyyx _ = False

supportTLS :: Address -> Bool
supportTLS ad = not (any isxyyx (snd ad)) && any isxyyx (fst ad)

-- Part 1
part1 :: [String] -> Int
part1 = length . filter supportTLS . rights . map (parse parseAddress "")

abas :: String -> [String]
abas (a:b:c:d) | a == c && a /= b = [a,b,c] : abas ([b, c] ++ d)
               | otherwise = abas ([b, c] ++ d)
abas _ = []

makeABASet :: String -> Set.Set String
makeABASet = Set.fromList . abas

addressSummary :: Address -> (Set.Set String, Set.Set String)
addressSummary = both (mconcat . map makeABASet)

abaTobab :: String -> String
abaTobab (a:b:_) = [b,a,b]
abaTobab _ = ""

supportSSL :: Address -> Bool
supportSSL = (0 < ) . Set.size . uncurry Set.intersection . fmap (Set.map abaTobab) . addressSummary

-- Part 2
part2 :: [String] -> Int
part2 = length . filter supportSSL . rights . map (parse parseAddress "")

main :: IO()
main = readInput >>= \content -> 
       print (part1 content) >>
       print (part2 content) >>
       return ()

readInput :: IO [String]
readInput = lines <$> readFile "input"

testCase1 :: [String]
testCase1 = ["abba[mnop]qrst", -- True
            "abcd[bddb]xyyx", -- False
            "aaaa[qwer]tyui", -- False
            "ioxxoj[asdfgh]zxcvbn" -- True
            ] 

testCase2 :: [String]
testCase2 = ["aba[bab]xyz",
            "xyx[xyx]xyx",
            "aaa[kek]eke",
            "zazbz[bzb]cdb"
            ]