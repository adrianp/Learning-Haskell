multThree :: Int -> (Int -> (Int -> Int))
multThree x y z = x * y * z

-- will take two arguments and multiply their product with 9
multTwoWithNine :: Int -> Int -> Int
multTwoWithNine = multThree 9

compareWithHundred :: Int -> Ordering
--compareWithHundred x = compare 100 x
compareWithHundred = compare 100

divideByTen :: (Floating a) => a -> a
divideByTen = (/10)

isUpperCase :: Char -> Bool
isUpperCase = (`elem` ['A'..'Z'])

subtractFour :: Integer -> Integer
subtractFour = (subtract 4)

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

add :: Num a => a -> a -> a
add x = (+x)

-- zipWith f [a, b] [x, y] = [f a x, f b y]
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

-- swaps the arguments of a function
flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f x y = f y x

flip'' :: (a -> b -> c) -> (b -> a -> c)
flip'' f = \x y -> f y x
