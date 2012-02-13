import qualified Data.List as DL 
import qualified Data.Char as DC
import qualified Data.Map as DM

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

digitSum :: Int -> Int
digitSum x
   | x < 0 = digitSum (-x) 
   | otherwise = sum $ map DC.digitToInt $ show x

findDigitSum :: Int -> Maybe Int
findDigitSum x = DL.find (\ y -> digitSum y == x) [1..] 

-- finds all tuples of type (key, value) for a given key
-- not a real Map, but still
findKey :: Eq a => [(a, b)] -> a -> [(a, b)]
findKey xs k = filter (\ x -> fst x == k) xs

-- finds only the first tuple of type (key value) for a given key
-- the real Map behaviour
findKey' :: (Eq k) => [(k,v)] -> k -> Maybe v
findKey' [] _ = Nothing
findKey' ((k,v):xs) key
    | key == k = Just v
    | otherwise = findKey' xs key 

findKey'' :: (Eq k) => [(k,v)] -> k -> Maybe v
findKey'' xs key = foldr (\ (k,v) acc -> if k == key then Just v else acc) Nothing xs 

phoneBook :: DM.Map String String
phoneBook = DM.fromList $
    [("adrian", "123-456"),
     ("john", "000-6147"),
     ("penny", "987-654")
    ]

string2digits :: String -> [Int]
string2digits s = map DC.digitToInt . filter DC.isDigit $ s

phoneBookToMap :: (Ord k) => [(k, String)] -> DM.Map k String
phoneBookToMap xs = DM.fromListWith addNumbers xs
    where addNumbers n1 n2 = n1 ++ ", " ++ n2

phoneBookToMap' :: (Ord k) => [(k,a)] -> DM.Map k [a]
phoneBookToMap' xs = DM.fromListWith (++) $ map (\ (k, v) -> (k, [v])) xs

phoneBookList :: [([Char], [Char])]
phoneBookList = [("adrian", "123-456"),
                 ("adrian", "123-555"),
                 ("john", "000-6147"),
                 ("penny", "987-654")
                ]
