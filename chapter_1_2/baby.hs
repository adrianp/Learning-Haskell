doubleMe :: Num a => a -> a
doubleMe x = x + x

--doubleUs x y = x * 2 + y * 2

doubleUs :: Num a => a -> a -> a
doubleUs x y = doubleMe x + doubleMe y

--doubleSmallNumber x = if x > 100
--                         then x
--                         else x*2

doubleSmallNumber :: (Ord a, Num a) => a -> a
doubleSmallNumber x = if x > 100
                         then x
                         else x+x

doubleSmallNumber' :: (Ord a, Num a) => a -> a
doubleSmallNumber' x = (if x > 100 then x else 2*x) + 1

conanO'Brien = "It's a-me, Conan O'Brien!"

boomBangs :: Integral a => [a] -> [[Char]] 
boomBangs xs = [if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]

length' :: [t] -> Int 
length' xs = sum [1 | x <- xs]

removeNonUppercase :: [Char] -> [Char]
removeNonUppercase st = [c | c <- st, elem c ['A'..'Z']]

-- removes odds from lists inside a list
removeOddsInner :: Integral a => [[a]] -> [[a]] 
removeOddsInner xxs = [[ x | x <- xs, even x ] | xs <- xxs]

addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

-- This example is on page 72, Chapter 5
addThree' :: Int -> Int -> Int -> Int
addThree' = (\x -> (\y -> (\z -> x + y + z)))

slowFactorial :: Integer -> Integer
slowFactorial n = product [1..n]

-- Int is faster than Integral
--fastFactorial :: Int -> Int
--fastFactorial n = product [1..n]

circumference :: Float -> Float
circumference r = 2 * pi * r

circumference' :: Double -> Double
circumference' r = 2 * pi * r
