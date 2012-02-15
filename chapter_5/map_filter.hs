map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs 

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
    | p x = x : filter' p xs
    | otherwise = filter' p xs

largestDivisible :: Integer -> Integer -> Integer
largestDivisible d max =
    let p x = mod x d == 0
    in head (filter' p [max,max-1..])

collatzChain :: Integral a => a -> [a]
collatzChain 1 = [1]
collatzChain x 
    | even x = x : collatzChain (div x 2)
    | odd x = x : collatzChain (3 * x + 1) 

-- get number of Collatz chains of specified length for numbers lower than max
numLongChains :: Integral a => a -> Int -> Int 
numLongChains max len = 
    let p xs = length xs > len
    in length (filter' p (map collatzChain [1..max]))

numLongChains' :: Integral a => a -> Int -> Int 
numLongChains' max len = length (filter' (\xs -> length xs > len) (map collatzChain [1..max]))
