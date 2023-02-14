{-# OPTIONS_GHC -Wall #-}
module Karahaiev02 where

-- Задача 1 -----------------------------------------
sumFl :: [Integer] -> Integer
sumFl xs = foldl (+) 0 xs 
  
-- Задача 2 ----------------------------------------- 
productFr :: [Integer] -> Integer
productFr xs = foldr (*) 1 xs

-- Задача 3 -----------------------------------------
concatFr :: [Int] -> [Int] -> [Int]
concatFr xs ys = foldr (:) ys xs 


insert :: Int -> [Int] -> [Int]
insert v xs = if null xs then [v] else if v <= head xs then v:xs else head xs : insert v (tail xs) 

-- Задача 4 -----------------------------------------
sortInsert :: [Int] -> [Int]
sortInsert xs = foldr insert [] xs

-- findIndices odd [5,7,6,4,3] = [0,1,4]
-- 5:[7:[6:[4:[3:[]]]]] = [5,7,6,4,3]
-- Задача 5 -----------------------------------------
findIndices ::(Int -> Bool) -> [Int] -> [Int] 
findIndices p xs = foldr (\y ys -> if p y then (length ys) : ys else ys) [] xs


-- Задача 6 -----------------------------------------
allReverse :: [String] -> [String]
allReverse xss = foldl (\ys xs -> (reverse xs) : ys) [] xss

-- Задача 7  -----------------------------------------
noDigits :: String -> String
noDigits = filter (\x -> not (elem x ['0'..'9']))

-- test = [(>0), odd, (<12), (/=5)]
-- Задача 8 ------------------------------------------
cntGood :: [Int -> Bool] -> Int -> Int
cntGood ps v = foldr (\p n -> if p v then n+1 else n) 0 ps 
 
-- Задача 9 ------------------------------------------
trianglePas :: [[Integer]]
trianglePas = iterate (\xs -> zipWith (+) (0 : xs) (xs ++ [0])) [1]

factorial :: Integer -> Integer
factorial n = if n == 0 then 1 else n * factorial (n-1)
-- Задача 10 -----------------------------------------
factorialsM :: [Integer]
factorialsM = 1:zipWith (*) [2..] factorialsM
