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
