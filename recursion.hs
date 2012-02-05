maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum': empty list"
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)

--replicate' :: Int -> a -> [a]
--replicate' 0 x = []
--replicate' 1 x = [x]
--replicate' n x = x:(replicate (n-1) x)

replicate'' :: Int -> a -> [a]
replicate'' n x
    | n <= 0 = []
    | otherwise = x : replicate'' (n-1) x

take' :: Int -> [a] -> [a]
take' n _
    | n <= 0 = []
take' _ [] = []
--take' n xs = (head xs) : take' (n-1) (tail xs)
take' n (x:xs) = x : take' (n-1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

repeat' :: a -> [a]
repeat' x = x : repeat' x

zip' :: [a] -> [b] -> [(a,b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys

elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' x (y:ys)
    | x == y = True
    | otherwise = elem' x ys

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = 
    let lte = [y | y <- xs, y <= x]
        gt = [y| y <- xs, y > x]
    in quicksort lte ++ [x] ++ quicksort gt

quicksort' :: (Ord a) => [a] -> [a]
quicksort' [] = []
quicksort' (x:xs) = 
    let lte = filter (<= x) xs
        gt = filter (> x) xs
    in quicksort' lte ++ [x] ++ quicksort' gt
