{-# OPTIONS_GHC -Wall #-}
module Karahaiev01 where

-- Задача 1 -----------------------------------------
power3 :: [Integer]
power3 = [x ^ 3 | x <- [1 ..]]

-- Задача 2 -----------------------------------------
toPower3 :: [Integer]
toPower3 = [3 ^ x | x <- [1 ..]]

-- Задача 3 -----------------------------------------
sumPower3 :: Integer -> Integer
sumPower3 x = sum ([3 ^ y | y <- [1 ..x]])

-- Задача 4 -----------------------------------------
sumPower :: Integer -> Integer -> Integer
sumPower m n = if n == 0 then 0 else sum ([m ^ x | x <- [1 ..n]])

-- Задача 5 -----------------------------------------
lessMe :: [Int] -> [Int]
lessMe xs = [length [y | y <- xs, y < x] | x <- xs]

-- Задача 6 -----------------------------------------
hailstone :: Int -> Int
hailstone n = if even n then n `div` 2 else 3 * n + 1

-- Задача 7 -----------------------------------------
hailSeq :: Int -> [Int]
hailSeq n = if n == 1 then [1] else n : hailSeq (hailstone n)

-- Задача 8 -----------------------------------------
allHailSeq :: [[Int]]
allHailSeq = [hailSeq x | x <- [1 ..]]

-- Задача 9 -----------------------------------------
firstHailSeq :: Int -> Int
-- firstHailSeq l = head [head x | x <- allHailSeq, length (x) == l]
firstHailSeq l = head [x | x <- [1 ..], length (hailSeq x) == l]