{-# OPTIONS_GHC -Wall #-}
module Practice1 where

add5 :: Int -> Int
add5 x = x + 5

task3 :: Int -> Int
task3 x = if x < 100 then x * 2 else x

task4 :: [Int] -> [Int] -> [Int]
task4 xs ys = if null xs then ys else head xs : task4 (tail xs) ys

concatM :: [[Int]] -> [Int]
concatM xs = if null xs then [] else head xs ++ concatM (tail xs)

reverseM :: [Int] -> [Int]
reverseM xs = if null xs then [] else reverseM(tail xs) ++ [head xs]