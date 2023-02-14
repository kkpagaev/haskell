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


insert :: [Int] -> Int -> [Int]
insert xs v = foldr (\x acc -> if x < v then x : acc else v : x : acc) [] xs

-- Задача 4 -----------------------------------------
sortInsert :: [Int] -> [Int]
sortInsert xs = foldr insert [] xs

-- Задача 5 -----------------------------------------
findIndices ::(Int -> Bool) -> [Int] -> [Int] 
findIndices  = undefined

-- Задача 6 -----------------------------------------
allReverse :: [String] -> [String]
allReverse = undefined

-- Задача 7  -----------------------------------------
noDigits :: String -> String
noDigits = undefined

-- Задача 8 ------------------------------------------
cntGood :: [Int -> Bool] -> Int -> Int
cntGood = undefined

-- Задача 9 ------------------------------------------
trianglePas :: [[Integer]]
trianglePas = undefined

-- Задача 10 -----------------------------------------
factorialsM :: [Integer]
factorialsM = undefined

