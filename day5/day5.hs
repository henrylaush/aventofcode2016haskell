import Data.ByteString.Char8 (pack)
import Data.List (isPrefixOf, lookup, foldl', sort)
import Crypto.Hash
import Data.Char (isDigit)
import Data.Maybe (isNothing)

tryHash :: String -> Int -> String
tryHash pre num = show $ md5 $ pack $ pre ++ show num
                where md5 x = hash x :: Digest MD5

isValid :: String -> Bool
isValid = isPrefixOf "00000"

getPassword :: String -> String
getPassword doorID = map (!!5) $ take 8 $ filter isValid $ map (tryHash doorID) [0..]

-- Part 1
part1 :: String -> String
part1 = getPassword

withinRange :: Char -> Bool
withinRange x = isDigit x && nx >= 0 && nx < 8
              where nx = read [x] :: Int

isValidP2 :: String -> Bool
isValidP2 s = isValid s && withinRange (s !! 5)

toPair :: String -> (Char, Char)
toPair s = (s !! 5, s !! 6)

addPasswordChar :: [(Char, Char)] -> (Char, Char) -> [(Char, Char)]
addPasswordChar ls p@(x, _)
    | length ls < 8 = case lookup x ls of Just _ -> ls
                                          Nothing -> ls ++ [p]
    | otherwise = ls

collectChar :: [(Char, Char)] -> [(Char, Char)]
collectChar = foldl' addPasswordChar []

p2PosCharPair :: String -> [(Char, Char)]
p2PosCharPair str = map toPair $ filter isValidP2  $ map (tryHash str) [0..]

lengthEightOnly :: [[a]] -> [[a]]
lengthEightOnly = filter ( (8 ==) . length)

-- Part 2
part2 :: String -> String
part2 doorId = map snd $ sort $ head $ lengthEightOnly $ map (\x -> collectChar $ take x $ p2PosCharPair doorId) [8..]

main :: IO()
main = print (part1 input) >>
       print (part2 input) 

input :: String
input = "ffykfhsq"