import qualified Data.List as DL 
import qualified Data.Char as DC

numUniques :: (Eq a) => [a] -> Int
numUniques = length . DL.nub

wordNums :: String -> [(String,Int)]
wordNums s =
    let xs = DL.group . DL.sort . DL.words $ s
    in [(head x, length x) | x <- xs]

wordNums' :: String -> [(String,Int)]
wordNums' = map (\ ws -> (head ws, length ws)) . DL.group . DL.sort . DL.words


wordNums'' :: String -> [(String,Int)]
wordNums'' xs = map (\ ws -> (head ws, length ws)) (DL.group (DL.sort (DL.words xs)))

-- same as Data.List.isInfixOf
isIn :: (Eq a) => [a] -> [a] -> Bool
isIn needle haystack = any (DL.isPrefixOf needle) (DL.tails haystack)

encode :: Int -> String -> String
encode offset s = map DC.chr (map (\ c -> DC.ord c + offset) s)

encode' :: Int -> String -> String
encode' offset msg = map (\ c -> DC.chr $ DC.ord c + offset) msg

decode :: Int -> String -> String
decode shift msg = encode (negate shift) msg
