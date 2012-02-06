-- foldr f z [1,2,3] = f 1 (f 2 ( f 3 z))
-- foldl f [1,2,3] z = f ( f (f z 1) 2) 3

sum' :: (Num a) => [a] -> a
sum' xs = foldl (\ acc x -> acc + x) 0 xs

--Remember that:
--foo a = bar b a
--is equivalent to
--foo = bar b

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

and' :: [Bool] -> Bool
and' xs = foldr (&&) True xs
