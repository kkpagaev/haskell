{-# OPTIONS_GHC -Wall #-}

module Karahaiev04 where

import Data.List

type GraphS = (Int, [(Int, Int)])

type Graph = [[Int]]

adj :: Graph -> Int -> [Int]
adj g v = g !! v

-- всі вершини графа g
nodes :: Graph -> [Int]
nodes g = [0 .. (length g - 1)]

goNodes :: Graph -> Int -> [Int]
goNodes gr v = snd $ until (\(ns, _) -> ns == []) (oneStep gr) ([v], [])

oneStep :: Graph -> ([Int], [Int]) -> ([Int], [Int])
oneStep gr (ns, os) =
  let old = ns ++ os
      ns1 = [n | v <- ns, n <- gr !! v]
      ns2 = filter (`notElem` old) ns1
      new = nub ns2
   in (new, old)

-- ребро належить графу
edgeIn :: Graph -> (Int, Int) -> Bool
edgeIn g (x, y) = elem y (g !! x) -- g!!x …. adj g x
-- всі ребра графу

edges :: Graph -> [(Int, Int)]
edges g = [(x, y) | x <- nodes g, y <- g !! x] -- g!!x …. adj g x

subsets :: [Int] -> [[Int]]
subsets (x : xs) =
  let xss = subsets xs
   in map (x :) xss ++ xss

allWays :: Graph -> Int -> [[[Int]]]
allWays gr v = until condW (stepW gr) [[[v]]]

condW :: ([[[Int]]]) -> Bool
condW wss = null (head wss)

stepW :: Graph -> [[[Int]]] -> [[[Int]]]
stepW gr wss@(wsn : _) =
  [t : w | w@(x : xs) <- wsn, notElem x xs, t <- gr !! x] : wss
stepW gr [] = error "allWays:stepW"

-- [0:[1:[2:[3:[]]]]] = [[0,1,2,3]]
-- Задача 1 ------------------------------------
isOrdinary :: Graph -> Bool
isOrdinary gr =
  null (loops gr)
    && all
      ( \(i, adj) ->
          if i < 0 || i >= length gr
            then False
            else
              all
                ( \v ->
                    (length [y | y <- gr !! v, y == i]) == 1
                )
                adj
      )
      (zip [0 ..] gr)

multipleEdges :: Graph -> [Int]
multipleEdges gr =
  let zipped = zip [0 ..] (nodes gr)
   in map fst (filter (\(x, y) -> length (filter (== y) (nodes gr)) > 1) zipped)

loops :: Graph -> [Int]
loops gr = [i | (i, xs) <- zip [0 ..] gr2, i `elem` xs]

-- Задача 2 ------------------------------------
fromGraph :: Graph -> GraphS
fromGraph gr = (length gr - 1, edges gr)

-- Задача 3 ------------------------------------
toGraph :: GraphS -> Graph
toGraph gs = [[y | (x, y) <- snd gs, x == v] | v <- [0 .. fst gs]]

allPaths :: Graph -> Int -> Int -> [[Int]]
allPaths gr a b = [p | p <- concat (allWays gr a), head p == b]

shortWay :: Graph -> Int -> Int -> [Int]
shortWay gr a b =
  if a == b
    then []
    else
      let paths = [y | x <- allWays gr a, y <- (reverse x), head y == b]
       in if null paths
            then []
            else reverse (last paths)

-- Задача 5 ------------------------------------
isConnecting :: Graph -> Bool
isConnecting gr = null (filter (not . null) (map (\p -> filter (null) ([(shortWay gr p p2) | p2 <- [0 .. length gr - 1], not (p2 == p)])) ([0 .. length gr - 1])))

-- Задача 6 ------------------------------------
merge :: [Int] -> [Int] -> [Int]
merge xs ys = xs ++ (filter (\x -> not (elem x xs)) ys)

components :: Graph -> [[Int]]
components gr =
  reverse
    ( foldl
        ( \xss v ->
            if elem v (concat xss)
              then xss
              else (sort (goNodes gr v)) : xss
        )
        []
        (nodes gr)
    )

-- Задача 7 ------------------------------------
eccentricity :: Graph -> Int -> Int
eccentricity gr a = maximum (map (\p -> length (shortWay gr a p) - 1) [0 .. length gr - 1])

-- Задача 8 ------------------------------------
findDiameter :: Graph -> Int
findDiameter gr = maximum (map (\p -> eccentricity gr p) [0 .. length gr - 1])

findRadius :: Graph -> Int
findRadius gr = minimum (map (\p -> eccentricity gr p) [0 .. length gr - 1])

-- Задача 9 ------------------------------------
findCenter :: Graph -> [Int]
findCenter gr = filter (\p -> eccentricity gr p == findRadius gr) [0 .. length gr - 1]

-- Задача 10 ------------------------------------
shortWayAll :: Graph -> Int -> Int -> [[Int]]
shortWayAll gr a b =
  if a == b
    then []
    else
      let paths = (allPaths gr b a)
       in if null paths
            then []
            else filter (\p -> length p == length (last paths)) paths

---------------------Тестові дані - Графи -------
gr1S, gr2S :: GraphS
gr1S =
  ( 5,
    [ (0, 1),
      (0, 2),
      (0, 3),
      (1, 0),
      (1, 3),
      (1, 4),
      (2, 0),
      (2, 4),
      (2, 5),
      (3, 0),
      (3, 1),
      (4, 1),
      (4, 2),
      (5, 2)
    ]
  )
gr2S =
  ( 7,
    [ (0, 1),
      (0, 3),
      (1, 0),
      (1, 2),
      (2, 1),
      (2, 3),
      (3, 0),
      (3, 2),
      (4, 5),
      (4, 6),
      (5, 4),
      (5, 6),
      (6, 4),
      (6, 5)
    ]
  )

gr1, gr2 :: Graph
gr1 = [[1, 2, 3], [0, 3, 4], [0, 4, 5], [0, 1], [1, 2], [2]]
gr2 = [[1, 3], [0, 2], [1, 3], [0, 2], [5, 6], [4, 6], [4, 5], []]