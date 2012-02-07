-- foldr f z [1,2,3] = f 1 (f 2 ( f 3 z))
-- foldl f [1,2,3] z = f ( f (f z 1) 2) 3

sum' :: (Num a) => [a] -> a
sum' xs = foldl (\ acc x -> acc + x) 0 xs

sum'' :: (Num a) => [a] -> a
sum'' = foldl (+) 0

-- foo a = bar b a <=> foo = bar b

map'' :: (a -> b) -> [a] -> [b]
map'' f xs = foldr (\ x acc -> f x : acc) [] xs

-- ++ is slower than :
map''' :: (a -> b) -> [a] -> [b]
map''' f xs = foldl (\ acc x -> acc ++ [f x]) [] xs

elem'' :: (Eq a) => a -> [a] -> Bool
elem'' x xs = foldr (\ y acc -> if x == y then True else acc) False xs

--max 1 2 = 2
maximum'' :: (Ord a) => [a] -> a
maximum'' = foldl1 max

reverse'' :: [a] -> [a]
-- we cannot do [] : x but we can x : []
reverse'' = foldl (\ acc x -> x : acc) [] 

reverse''' :: [a] -> [a]
reverse''' = foldl (flip (:)) []


product' :: (Num a) => [a] -> a
product' = foldl (*) 1

filter'' :: (a -> Bool) -> [a] -> [a]
filter'' p = foldr (\ x acc -> if p x then x : acc else acc) []

last' :: [a] -> a
last' = foldl1 (\ _ x -> x)

last'' :: [a] -> a
last'' xs = xs !! (length xs -1)

and' :: [Bool] -> Bool
and' xs = foldr (&&) True xs

-- argument must be float as we compare it with floats
sqrtSums :: Float -> Int
sqrtSums limit = length (takeWhile (<limit) $ scanl1 (+) $ map sqrt [1..]) + 1

-- f a b c = ((f a) b) c
-- f $ g $ x = f $ (g $ x)
-- f (g (z x)) = (f . g . z) x

aFun = ceiling . negate . tan . cos . max 50

oddSquareSum :: Integral a => a -> a 
oddSquareSum limit = sum . takeWhile (<limit) . filter odd $ map (^2) [1..]
