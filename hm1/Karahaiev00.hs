{-# OPTIONS_GHC -Wall #-}

module Karahaiev00 where

data Quaternion = Quaternion Double Double Double Double
  deriving (Eq)

type Graph = [[Int]]

data Tree23 a
  = Leaf a
  | Fork2 (Tree23 a) a (Tree23 a)
  | Fork3 (Tree23 a) a (Tree23 a) a (Tree23 a)
  | Null23 -- порожнє 2-3-дерево!!!
  deriving (Eq, Show)

-- Задача  1 -----------------------------------------
instance Show Quaternion where
  show (Quaternion a b c d) = show a ++ bSign ++ show b ++ "i" ++ cSign ++ show c ++ "j" ++ dSign ++ show d ++ "k"
    where
      bSign = if b < 0 then "" else "+"
      cSign = if c < 0 then "" else "+"
      dSign = if d < 0 then "" else "+"

-- Задача 2 -----------------------------------------
plusQuaternion :: Quaternion -> Quaternion -> Quaternion
plusQuaternion q1 q2 =
  let (Quaternion a1 b1 c1 d1) = q1
      (Quaternion a2 b2 c2 d2) = q2
   in Quaternion (a1 + a2) (b1 + b2) (c1 + c2) (d1 + d2)

-- Задача 3 -----------------------------------------
timesQuaternion :: Quaternion -> Quaternion -> Quaternion
timesQuaternion q1 q2 =
  let (Quaternion a1 b1 c1 d1) = q1
      (Quaternion a2 b2 c2 d2) = q2
   in Quaternion (a1 * a2 - b1 * b2 - c1 * c2 - d1 * d2) (a1 * b2 + b1 * a2 + c1 * d2 - d1 * c2) (a1 * c2 - b1 * d2 + c1 * a2 + d1 * b2) (a1 * d2 + b1 * c2 - c1 * b2 + d1 * a2)

--- Задача 4 ----------------------------------------
instance Num Quaternion where
  (+) = plusQuaternion
  (*) = timesQuaternion
  negate = negateQuaternion
  fromInteger = fromIntegerQuaternion
  abs = absQuaternion
  signum = signumQuaternion

negateQuaternion :: Quaternion -> Quaternion
negateQuaternion (Quaternion a b c d) = Quaternion (- a) (- b) (- c) (- d)

fromIntegerQuaternion :: Integer -> Quaternion
fromIntegerQuaternion n = Quaternion (fromInteger n) 0 0 0

absQuaternion :: Quaternion -> Quaternion
absQuaternion (Quaternion a b c d) = Quaternion (sqrt (a * a + b * b + c * c + d * d)) 0 0 0

signumQuaternion :: Quaternion -> Quaternion
signumQuaternion (Quaternion a b c d) = Quaternion (a / sq) (b / sq) (c / sq) (d / sq)
  where
    sq = sqrt (a * a + b * b + c * c + d * d)

-- Задача 5 -----------------------------------------
isTournament :: Graph -> Bool
isTournament gr =
  let nodes = [0 .. (length gr - 1)]
      count = length gr - 1
   in all (\n -> length (connected gr n) == count) nodes

connected :: Graph -> Int -> [Int]
connected gr n =
  let forwards = gr !! n
      ngr = [nds | (i, nds) <- zip [0 ..] gr, i /= n]
      backwards = [i | (i, nds) <- zip [0 ..] ngr, elem n nds]
   in forwards ++ backwards

-- Задача 6 -----------------------------------------
gamiltonWay :: Graph -> Maybe [Int]
-- gamiltonWay gr = let nodes = [0..(length gr - 1)]
--                   in if isTournament gr then Just (gamilton gr nodes) else Nothing
gamiltonWay gr =
  let ways = (gamilton gr)
   in if ways == [] then Nothing else Just (last ways)

gamilton gr = filter (\w -> (head w) == 0 && (last w) == 0) (map reverse (filter (\way -> length way == (length gr + 1)) ways))
  where
    ways = (concat $ concat [allWays gr n | n <- [0 .. (length gr - 1)]])

oneStep :: Graph -> ([Int], [Int]) -> ([Int], [Int])
oneStep gr (ns, os) =
  let old = ns ++ os
      ns1 = [n | v <- ns, n <- gr !! v]
      ns2 = filter (`notElem` old) ns1
      new = nub ns2
   in (new, old)

nub :: [Int] -> [Int]
nub [] = []
nub (x : xs) = x : nub (filter (/= x) xs)

goNodes :: Graph -> Int -> [Int]
goNodes gr v = snd $ until (\(ns, _) -> ns == []) (oneStep gr) ([v], [])

-- Задача 7 -----------------------------------------
isAcyclic :: Graph -> Bool
isAcyclic gr =
  let nodes = [0 .. (length gr - 1)]
   in not (any (hasCycle gr) nodes)

hasCycle gr n =
  let gos = goNodes gr n
   in any
        ( \go ->
            let edges = gr !! go
             in n `elem` edges
        )
        (gos)

-- Задача 8 -----------------------------------------
isTopolSort :: Graph -> [Int] -> Bool
isTopolSort gr ts =
  let nodes = [0 .. (length gr - 1)]
      edges = concat gr
   in (length ts == length nodes) && (all (\n -> n `elem` ts) nodes) && (all (\n -> n `elem` edges) ts)

allPaths :: Graph -> Int -> Int -> [[Int]]
allPaths gr a b = [p | p <- concat (allWays gr a), head p == b]

allWays :: Graph -> Int -> [[[Int]]]
allWays gr v = until condW (stepW gr) [[[v]]]

condW :: ([[[Int]]]) -> Bool
condW wss = null (head wss)

stepW :: Graph -> [[[Int]]] -> [[[Int]]]
stepW gr wss@(wsn : _) =
  [t : w | w@(x : xs) <- wsn, notElem x xs, t <- gr !! x] : wss
stepW gr [] = error "allWays:stepW"

-- Задача 9 -----------------------------------------
longWay :: Graph -> Int -> Int -> Maybe [Int]
longWay gr a b =
  if a == b
    then Nothing
    else
      let paths = (allPaths gr a b)
       in if null paths
            then Nothing
            else Just (reverse (head (filter (\p -> not (b `elem` (tail p))) paths)))

--- Задача 10 ----------------------------------------
merge :: [Int] -> [Int] -> [Int]
merge xs ys = sortb $ xs ++ (filter (\x -> not (elem x xs)) ys)

sortb :: [Int] -> [Int]
sortb [] = []
sortb (x : xs) = sortb (filter (< x) xs) ++ [x] ++ sortb (filter (>= x) xs)

--- Задача 11 ----------------------------------------
intToString :: Int -> Int -> String
intToString n m = if n < m then [intToChar n] else intToString (n `div` m) m ++ [intToChar (n `mod` m)]

intToChar :: Int -> Char
intToChar n = if n < 10 then toEnum (n + 48) else toEnum (n + 87)

--- Задача 12 ----------------------------------------
stringToInt :: Int -> String -> Maybe Int
stringToInt n xs = if all (\x -> elem x (baseDict n)) xs then Just (stringToInt' n xs) else Nothing

baseDict :: Int -> [Char]
baseDict 2 = ['0', '1']
baseDict 3 = ['0', '1', '2']
baseDict 4 = ['0', '1', '2', '3']
baseDict 5 = ['0', '1', '2', '3', '4']
baseDict 6 = ['0', '1', '2', '3', '4', '5']
baseDict 7 = ['0', '1', '2', '3', '4', '5', '6']
baseDict 8 = ['0', '1', '2', '3', '4', '5', '6', '7']
baseDict 9 = ['0', '1', '2', '3', '4', '5', '6', '7', '8']
baseDict 10 = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']
baseDict 11 = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a']
baseDict 12 = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b']
baseDict 13 = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c']
baseDict 14 = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd']
baseDict 15 = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e']
baseDict 16 = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f']
baseDict _ = []

stringToInt' :: Int -> String -> Int
stringToInt' n (x : xs) = if length xs == 0 then charToInt x else (charToInt x) * n ^ (length xs) + stringToInt' n xs

charToInt :: Char -> Int
charToInt c = if c `elem` ['0' .. '9'] then fromEnum c - 48 else fromEnum c - 87

--- Задача 13 ----------------------------------------
data Operation = Plus | Minus | Times deriving (Eq)

instance Show Operation where
  show Plus = "+"
  show Minus = "-"
  show Times = "*"

genExpr :: Int -> Int -> [String]
genExpr a b =
  let nums = map charToInt (intToString a 10)
      conbs = permuGenExpr (length nums - 1)
      res = filter (\conb -> ((evalExpr nums conb) == b)) conbs
   in map (mergeGenExpr nums) res

mergeGenExpr (x : xs) (y : ys) = show x ++ show y ++ mergeGenExpr xs ys
mergeGenExpr [] (y : ys) = show y ++ mergeGenExpr [] ys
mergeGenExpr (x : xs) [] = show x ++ mergeGenExpr xs []
mergeGenExpr [] [] = []

permuGenExpr :: Int -> [[Operation]]
permuGenExpr 1 = [[Plus], [Minus], [Times]]
permuGenExpr n = [x : xs | x <- [Plus, Minus, Times], xs <- permuGenExpr (n - 1)]

evalExpr :: [Int] -> [Operation] -> Int
evalExpr (a : _) [] = a
evalExpr (a : xs) (op : ops) =
  let b = head xs
      res = eval a b op
   in evalExpr (res : (tail xs)) ops

eval :: Int -> Int -> Operation -> Int
eval x y Plus = x + y
eval x y Minus = x - y
eval x y Times = x * y

--- Задача 14 ----------------------------------------
genExprBracket :: Int -> Int -> [String]
genExprBracket = undefined

--- Задача 15 ----------------------------------------
isTree23 :: (Ord a) => Tree23 a -> Bool
isTree23 t =
  let l = treeToList t
   in case l of
        Just l' -> isSorted l'
        Nothing -> False

isSorted :: (Ord a) => [a] -> Bool
isSorted [] = True
isSorted [x] = True
isSorted (x : y : xs) = if x <= y then isSorted (y : xs) else False

treeToList :: (Ord a) => Tree23 a -> Maybe [a]
treeToList Null23 = Just []
treeToList (Leaf x) = Just [x]
treeToList (Fork2 l x r) = case (treeToList l, treeToList r) of
  (Just l', Just r') -> Just (l' ++ [x] ++ r')
  _ -> Nothing
treeToList (Fork3 l x m y r) = case (treeToList l, treeToList m, treeToList r) of
  (Just l', Just m', Just r') -> Just (l' ++ [x] ++ m' ++ [y] ++ r')
  _ -> Nothing

--- Задача 16 ----------------------------------------
eqTree23 :: (Ord a) => Tree23 a -> Tree23 a -> Bool
eqTree23 t1 t2 =
  let l1 = treeToList t1
      l2 = treeToList t2
   in case (l1, l2) of
        (Just l1', Just l2') -> l1' == l2'
        _ -> False

--- Задача 17 ----------------------------------------
elemTree23 :: (Ord a) => Tree23 a -> a -> Bool
elemTree23 Null23 _ = False
elemTree23 (Leaf x) y = x == y
elemTree23 (Fork2 l x r) y = if x == y then True else elemTree23 l y || elemTree23 r y
elemTree23 (Fork3 l x m y r) z = if x == z || y == z then True else elemTree23 l z || elemTree23 m z || elemTree23 r z

--- Задача 18 ----------------------------------------
insTree23 :: (Ord a) => Tree23 a -> a -> Tree23 a
insTree23 = undefined

-- isTerminal tr = True <=> якщо сини вузла tr - листки !!
isTerminal :: (Ord a) => Tree23 a -> Bool
isTerminal (Fork2 (Leaf _) _ _) = True
isTerminal (Fork3 (Leaf _) _ _ _ _) = True
isTerminal _ = False

-- Результат вставки вузла в 2-3-дерево,
--   корінь якого - вузол вида Fork2 або Fork3 є об`єкт із (Tree23 a, Maybe (a, Tree23 a))
--   : (a, Nothing) - результат вставки - одне 2-3-дерево a
--   : (a, Just (w, b)) - результат вставки два 2-3-дерева a i b (w - найменше значення в b)
--  insert v tr - додає значення v в довільне дерево tr
insert :: (Ord a) => a -> Tree23 a -> (Tree23 a, Maybe (a, Tree23 a))
insert v tr
  | isTerminal tr = insTerm v tr
  | otherwise = insFork v tr

-- insTerm v tr - додається значення v в дерево tr з конем - термінальний вузол
insTerm :: (Ord a) => a -> Tree23 a -> (Tree23 a, Maybe (a, Tree23 a))
insTerm = undefined

-- insFork v tr - додає значення v в дерево tr з корнем - нетермінальний вузол
insFork :: (Ord a) => a -> Tree23 a -> (Tree23 a, Maybe (a, Tree23 a))
insFork = undefined

---------------------Тестові дані

---------------------- Графи -------
gr1, gr2, gr3 :: Graph
gr1 = [[1, 2, 3], [2, 3], [3, 4], [4], []]
gr2 = [[3, 4], [0, 3], [0, 1, 4], [2, 4], [1]]
gr3 = [[1], [2], [3], [1], [0, 3]]

---------------------- 2-3-дерева
tr1, tr2, tr3, tr4, tr5 :: Tree23 Int
tr1 =
  Fork2
    ( Fork2
        (Fork2 (Leaf 0) 1 (Leaf 1))
        2
        (Fork2 (Leaf 2) 3 (Leaf 3))
    )
    4
    ( Fork2
        (Fork2 (Leaf 4) 5 (Leaf 5))
        6
        (Fork2 (Leaf 6) 7 (Leaf 7))
    )
tr2 =
  Fork3
    (Fork2 (Leaf 0) 1 (Leaf 1))
    2
    (Fork3 (Leaf 2) 3 (Leaf 3) 4 (Leaf 4))
    5
    (Fork3 (Leaf 5) 6 (Leaf 6) 7 (Leaf 7))
tr3 =
  Fork3
    (Fork2 (Leaf 2) 5 (Leaf 5))
    7
    (Fork3 (Leaf 7) 8 (Leaf 8) 12 (Leaf 12))
    16
    (Fork2 (Leaf 16) 19 (Leaf 19))
tr4 =
  Fork3
    (Fork2 (Leaf 2) 5 (Leaf 5))
    7
    (Fork3 (Leaf 7) 8 (Leaf 8) 12 (Leaf 12))
    16
    (Fork3 (Leaf 16) 18 (Leaf 18) 19 (Leaf 19))
tr5 =
  Fork2
    ( Fork2
        (Fork2 (Leaf 2) 5 (Leaf 5))
        7
        (Fork2 (Leaf 7) 8 (Leaf 8))
    )
    10
    ( Fork2
        (Fork2 (Leaf 10) 12 (Leaf 12))
        16
        (Fork3 (Leaf 16) 18 (Leaf 18) 19 (Leaf 19))
    )