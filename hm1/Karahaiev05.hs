{-# OPTIONS_GHC -Wall #-}
module Karahaiev05 where

import Data.List(nub,sort)

-- машина Тюрінга
data Going = No | Lt | Rt deriving (Show,Eq,Ord)
type Table = [((Int,Char),(Int,Char,Going))]
type Machine = (Int, Table)
type Config = (String,(Int,Char), String, Int)

-- опис складної машини Тюрінга 
-- L | R | P Char | G  базові машини
data Complex = Join [Complex] 
             | While Char Complex 
             | If Char Complex Complex 
             | P Char 
             | L | R | G 
             deriving (Show, Eq)

-- Задача 1.a -----------------------------------------
alphabet :: Table -> [Char]
alphabet t = (sort . nub . concat ) [[a,b] | ((_,a),(_,b,_)) <- t]

-- Задача 1.b ----------------------------------------- 
states :: Machine -> [Int]
states (i, t) = (sort . nub . concat) ([[i]] ++ [[ x | x <-[a, b], x /= 0] | ((a,_),(b,_,_)) <- t ])

-- Задача 2 -----------------------------------------
iswfMachine :: Machine -> Bool
iswfMachine m@(i,t) = let 
  ks = keys t
  alp = alphabet t
  sts = states m
    in null (dublicates ks) && (i `elem` sts) && any (\x -> not (x <= 0)) sts 
    && all (hasAllAlphabetDef ks alp) sts 

hasAllAlphabetDef :: [(Int, Char)] -> [Char] -> Int -> Bool
hasAllAlphabetDef ks alp i = let 
                        posible = [ (i, x) | x <- alp]
                        in null [ x | x <- posible, not (x `elem` ks)]
keys :: Table -> [(Int, Char)]
keys t = [a | (a,_) <- t]

dublicates :: [(Int, Char)] -> [(Int, Char)]
dublicates [] = []
dublicates (x:xs) = if x `elem` xs then x : dublicates xs else dublicates xs

initCon :: Machine -> String -> Config 
-- будує початкову конфігурацію за машиною і вхідним рядком 
initCon (is,_) ""     = ("", (is,' '), "", 0)
initCon (is,_) (c:cx) = ("", (is, c), cx, 0)

-- Задача 3.a -----------------------------------------
isFinal :: Int -> Config -> Bool
isFinal mx (_, (st, _), _, n) = n >= mx || st == 0

-- Задача 3.b -----------------------------------------
stepM :: Machine -> Config -> Config
stepM m (left, comb, after, stepCount) = 
  let (state, ch, go) = getStep m comb
  in stepM' go (left, (state, ch), after, stepCount + 1)

stepM' :: Going -> Config -> Config
stepM' No conf = conf
stepM' Rt (left, (st, ch), after, n) = (left ++ [ch], (st, safeHead after), safeTail after, n)
stepM' Lt (left, (st, ch), after, n) = (safeInit left, (st, safeLast left), ch : after, n)

safeInit :: String -> String
safeInit "" = ""
safeInit s = init s

safeHead :: String -> Char
safeHead "" = ' '
safeHead (x:_) = x

safeLast :: String -> Char
safeLast "" = ' '
safeLast (x:[]) = x
safeLast (_:xs) = safeLast xs

safeTail :: String -> String
safeTail "" = ""
safeTail (_:xs) = xs

getStep :: Machine -> (Int, Char) -> (Int, Char, Going)
getStep (_, t) (st, ch) = head [b | (a, b) <- t, a == (st, ch)]


-- Задача 4 -----------------------------------------
eval :: Machine -> Int -> String -> Maybe String
eval m mx u = let 
  c = initCon m u
  in if iswfMachine m then eval' m mx c  else Nothing

eval' :: Machine -> Int -> Config -> Maybe String
eval' m mx c = let 
    result@(_,(s,_),_,_) = until (isFinal mx) (stepM m) c
    in if s == 0 then Just (confString result) else Nothing

confString :: Config -> String
confString (left,(_,ch), right, _) = left ++ [ch] ++ right

-- concat two strings 
-- " " ++ "abc" = " abc"
-- Задача 5.a -----------------------------------------
renum :: Int -> Machine -> Machine   
renum k (i, t) = (i + k, [((a + k, b), (c, d, e)) | ((a, b), (c, d, e)) <- t]) 

-- Задача 5.b -----------------------------------------
connect :: Int -> Table -> Table
connect k t = [((a, b), (c + k, d, e)) | ((a, b), (c, d, e)) <- t]

-- Задача 6.a -----------------------------------------
seqJoin :: Machine -> Machine -> Machine 
seqJoin m1 (s2,t2) = let 
  (i1, ta1) = renum s2 m1
  ta2 = connect s2 ta1
  in (i1, sort(ta2 ++ t2)) 

-- Задача 6.b -----------------------------------------
ifJoin :: Char -> String -> Machine -> Machine -> Machine
ifJoin c alf m1 (is2, go2) = let 
  (isa, goa) = renum is2 m1
  is = isa + 1
  ta3 = [ ((is,s), (if s==c then isa else is2,s,No))  | s<-alf ]
  in (is, sort ( go2 ++ goa ++ ta3)) 

-- Задача 6.c -----------------------------------------
cycleJoin :: Char -> String -> Machine -> Machine 
cycleJoin c alf (is, go) = let 
       is1 = is+1
       go1 = connect is1 go
       ta3 = [ ((is,s), (if s==c then is else 0,s,No))  | s<-alf ]
       in (is1, go1 ++ ta3)
-- Задача 7 -----------------------------------------
build :: String -> Complex -> Machine
build = undefined    

-- Задача 8.a-----------------------------------------
subtractAbs :: Complex
subtractAbs = undefined

-- Задача 8.b-----------------------------------------
subtraction :: Complex     
subtraction = undefined

--------------------------------------------------------
--  тестові дані 
-- приклади машин Тюрінга 
test1, test2 :: Machine 
-- алфавіт " abc": знаходить перший символ 'a' заміняє на 'b' і зупиняється  
test1 = (1, [ ((1,'a'),(0,'b',No)), ((1,'b'),(1,'b',Rt))
            , ((1,'c'),(1,'c',Rt)), ((1,' '),(1,' ',Rt))])
-- алфавіт " a": невизначена функція переходу в (1,'a') !!! 
test2 = (2,[((2,'a'),(2,'a',Rt)),((2,' '),(1,' ',Rt)),((1,' '),(0,' ',Rt))])

-- будуємо складну машину з найпростіших
-- будуємо машину, що обчислює додавання
-- найпростіші . алфавіт == " #|"
rht, putO, putW :: Machine 
rht  = (1, map (\c->((1,c),(0,c,Rt))) " #|")   -- переміщує вправо
putO = (1, map (\c->((1,c),(0,'|',No))) " #|") -- записує символ '|'
putW = (1, map (\c->((1,c),(0,' ',No))) " #|") -- записує символ ' '

-- складніші машини 
rightO, rightM, main, additionM :: Machine 
rightO = cycleJoin '|' " #|" rht      -- проходить вправо всі '|'
rightM = seqJoin rht rightO           -- вправо завжди і потім вправо всі '|' 
main   = seqJoin (seqJoin putW rightM) putO  -- додавання, коли x>0
additionM = ifJoin '|' " #|" main putW       -- додавання, коли x>=0 

-- приклади побудов машин Тюрінга (обєкти типу Complex)
right, left, copy, addition :: Complex 
-- вправо завжди і потім вправо всі '|'
right = Join [R,While '|' R]
-- вліво завжди і потім вліво всі '|'  
left  = Join [L,While '|' L] 
-- додавання x+y 
addition = If '|' (Join [P ' ',right,P '|']) (P ' ')  
-- копіювання *|.x.| |.y.| ==> *|.x.| |.y+x.| 
copy = Join [While '|' (Join [P ' ',right,right,P '|',left,left,P '|',R])
            ,Join [left,R]
            ]

rightOT, rightMT, mainT, additionMT :: Machine 
rightOT = (2,
  [((1,' '),(2,' ',Rt)),((1,'#'),(2,'#',Rt)),((1,'|'),(2,'|',Rt))
  ,((2,' '),(0,' ',No)),((2,'#'),(0,'#',No)),((2,'|'),(1,'|',No))])
rightMT = (3,
  [((1,' '),(2,' ',Rt)),((1,'#'),(2,'#',Rt)),((1,'|'),(2,'|',Rt))
  ,((2,' '),(0,' ',No)),((2,'#'),(0,'#',No)),((2,'|'),(1,'|',No))
  ,((3,' '),(2,' ',Rt)),((3,'#'),(2,'#',Rt)),((3,'|'),(2,'|',Rt))])
mainT = (5,
  [((1,' '),(0,'|',No)),((1,'#'),(0,'|',No)),((1,'|'),(0,'|',No))
  ,((2,' '),(3,' ',Rt)),((2,'#'),(3,'#',Rt)),((2,'|'),(3,'|',Rt))
  ,((3,' '),(1,' ',No)),((3,'#'),(1,'#',No)),((3,'|'),(2,'|',No))
  ,((4,' '),(3,' ',Rt)),((4,'#'),(3,'#',Rt)),((4,'|'),(3,'|',Rt))
  ,((5,' '),(4,' ',No)),((5,'#'),(4,' ',No)),((5,'|'),(4,' ',No))])  
additionMT = (7,
  [((1,' '),(0,' ',No)),((1,'#'),(0,' ',No)),((1,'|'),(0,' ',No))
  ,((2,' '),(0,'|',No)),((2,'#'),(0,'|',No)),((2,'|'),(0,'|',No))
  ,((3,' '),(4,' ',Rt)),((3,'#'),(4,'#',Rt)),((3,'|'),(4,'|',Rt))
  ,((4,' '),(2,' ',No)),((4,'#'),(2,'#',No)),((4,'|'),(3,'|',No))
  ,((5,' '),(4,' ',Rt)),((5,'#'),(4,'#',Rt)),((5,'|'),(4,'|',Rt))
  ,((6,' '),(5,' ',No)),((6,'#'),(5,' ',No)),((6,'|'),(5,' ',No))
  ,((7,' '),(1,' ',No)),((7,'#'),(1,'#',No)),((7,'|'),(6,'|',No))])