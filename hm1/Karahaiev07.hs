{-# OPTIONS_GHC -Wall #-}
module Karahaiev07 where

data BinTreeM a = EmptyM 
                | NodeM a Int (BinTreeM a) (BinTreeM a)
                   deriving (Show, Eq) 
-- B-дерево порядка t (NodeB kl tl) =>  
--      t-1 <= length kl <= 2*t-1  &&  t <= length tl <= 2*t
data Btree a =  NodeB [a] [Btree a]  deriving (Show, Eq)
-- головні характеристики B-дерево  (BInform heigth min max)
data BInform a = BInform {hB::Int, minB::a, maxB::a} deriving (Show, Eq)

-- Задача 1 ------------------------------------
isSearch :: (Ord a) => BinTreeM a -> Bool
isSearch EmptyM = True
isSearch (NodeM v k lt rt) = k > 0 && isSearch lt && isSearch rt &&
                             (if lt == EmptyM then True else v > maxTree lt) &&
                             (if rt == EmptyM then True else v < minTree rt)

maxTree :: (Ord a) => BinTreeM a -> a
maxTree (NodeM v _ _ EmptyM) = v
maxTree (NodeM _ _ _ rt) = maxTree rt

minTree :: (Ord a) => BinTreeM a -> a
minTree (NodeM v _ EmptyM _) = v
minTree (NodeM _ _ lt _) = minTree lt


-- Задача 2 ------------------------------------
elemSearch :: (Ord a) => BinTreeM a -> a -> Bool
elemSearch EmptyM _ = False
elemSearch (NodeM v _ lt rt) x = elemSearch lt x || elemSearch rt x || x == v



-- Задача 3 ------------------------------------
insSearch :: (Ord a) => BinTreeM a -> a -> BinTreeM a 
insSearch EmptyM x = NodeM x 1 EmptyM EmptyM
insSearch (NodeM u k lt rt) x = if u > x then NodeM u k (insSearch lt x) rt
                                else if u < x then NodeM u k lt (insSearch rt x)
                                else NodeM u (k+1) lt rt

-- Задача 4 ------------------------------------
delSearch :: (Ord a) => BinTreeM a -> a -> BinTreeM a 
delSearch EmptyM _ = EmptyM
delSearch (NodeM u k lt rt) x = if u > x then NodeM u k (delSearch lt x) rt
                                else if u < x then NodeM u k lt (delSearch rt x)
                                else if k > 1 then NodeM u (k-1) lt rt
                                else if lt == EmptyM then rt
                                else if rt == EmptyM then lt
                                else NodeM (maxTree lt) 1 (delSearch lt (maxTree lt)) rt


-- Задача 5 ------------------------------------
sortList :: (Ord a) => [a] -> [a]
sortList xs = sortList' xs EmptyM

sortList' :: (Ord a) => [a] -> BinTreeM a -> [a]
sortList' [] tree = treeToList tree
sortList' (x:xs) tree = sortList' xs (insSearch tree x)

treeToList :: (Ord a) => BinTreeM a -> [a]
treeToList EmptyM = []
treeToList (NodeM v k lt rt) = treeToList lt ++ [v | _ <- [1..k]] ++ treeToList rt


-- Задача 6 ------------------------------------
findBInform :: (Bounded a, Ord a) => Btree a ->  BInform a
findBInform t = (BInform (findHeight t) (findMin t) (findMax t))

findHeight :: (Bounded a, Ord a) => Btree a -> Int
findHeight (NodeB _ t) = if t == [] then 0 else 1 + maximum (map findHeight t)

findMin :: (Bounded a, Ord a) => Btree a -> a
findMin (NodeB (x:xs) t) = if t == [] then x else findMin (head t)

findMax :: (Bounded a, Ord a) => Btree a -> a
findMax (NodeB (x:xs) t) = if t == [] then last xs else findMax (last t)

-- Задача 7 ------------------------------------
isBtree :: (Bounded a, Ord a) => Int -> Btree a -> Bool 
isBtree t tr@(NodeB kl tl) = if tl == [] then True 
   else (t-1 <= length kl && length kl <= 2*t-1) 
   && (t <= length tl && length tl <= 2*t) 
   && (all (isBtree t) tl) 
   && (all (\x -> x >= minB (findBInform tr) && x <= maxB (findBInform tr)) kl)


-- Задача 8 ------------------------------------
eqBtree :: (Bounded a, Ord a) => Int -> Btree a -> Btree a -> Bool 
eqBtree t t1 t2 = isBtree t t1 && isBtree t t2 && toList t1 == toList t2

toList :: (Ord a) => Btree a -> [a]
toList (NodeB kl tl) = if tl == [] then kl 
               else concat (zipWith (++) (map toList tl) ((map (\x ->[x]) kl) ++ [[]]))

-- Задача 9 ------------------------------------
elemBtree :: Ord a => Btree a -> a -> Bool
elemBtree tr v = elem v (toList tr)

position :: Ord a => a -> [a] -> Int
position = undefined

-- Задача 10 ------------------------------------
insBtree :: Ord a => Int -> Btree a -> a -> Btree a
insBtree = undefined

isFull :: Ord a => Int -> Btree a -> Bool
isFull = undefined

insertKey :: Ord a => a -> [a] -> [a]
insertKey = undefined

decomposeNodeB :: Ord a => a -> [a] -> [Btree a] -> 
                        ([a], [a], [Btree a], Btree a, [Btree a])
decomposeNodeB = undefined

splitAtB :: Ord a => Int -> Btree a -> (Btree a, a, Btree a)
splitAtB = undefined

---------------------Тестові дані - Дерева пошуку -------
bm :: BinTreeM Char
bm = NodeM  't' 2  
            (NodeM 'a' 1  EmptyM 
                    (NodeM 'e' 1 
                             (NodeM 'd' 2 EmptyM EmptyM)
                             (NodeM 'f' 1 EmptyM EmptyM)
                    )
            ) 
            (NodeM 'w' 2  EmptyM EmptyM)   

tBt1 :: Btree Char 
tBt1 = NodeB "L"
       [ NodeB "DG" 
          [ NodeB "AC" [], NodeB "EE" [], NodeB "HK" []
          ]
       , NodeB "PU" 
          [ NodeB "MM" [], NodeB "RS" [], NodeB "UW" []
          ]
       ]

tBt2 :: Btree Char 
tBt2 = NodeB "GP"
       [ NodeB "ACDEE" [], NodeB "HKLMM" [], NodeB "RSUUW" []
       ]

tBt5 :: Btree Char 
tBt5 = NodeB "GMPX"
       [ NodeB "ACDE" [] , NodeB "JK" [], NodeB "NO" []
       , NodeB "RSTUV" [], NodeB "YZ" []
       ]

tBt6 :: Btree Char 
tBt6 = NodeB "GMPX"
       [ NodeB "ABCDE" [], NodeB "JK" [], NodeB "NO" []
       , NodeB "RSTUV" [], NodeB "YZ" []
       ]

tBt7 :: Btree Char 
tBt7 = NodeB "GMPTX"
       [ NodeB "ABCDE" [], NodeB "JK" [], NodeB "NO" []
       , NodeB "QRS" [], NodeB "UV" [], NodeB "YZ" []
       ]

tBt8 :: Btree Char 
tBt8 = NodeB "P"
       [ NodeB "GM"
          [ NodeB "ABCDE" [], NodeB "JKL" [], NodeB "NO" []
          ]
       , NodeB "TX" 
          [ NodeB "QRS" [], NodeB "UV" [], NodeB "YZ" []
          ]
       ]

tBt9 :: Btree Char 
tBt9 = NodeB "P"
       [ NodeB "CGM"
          [ NodeB "AB" [], NodeB "DEF" []
          , NodeB "JKL" [], NodeB "NO" []
          ]
       , NodeB "TX" 
          [ NodeB "QRS" [], NodeB "UV" [], NodeB "YZ" []
          ]
       ]
