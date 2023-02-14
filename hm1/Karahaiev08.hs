{-# OPTIONS_GHC -Wall #-}

module Karahaiev08 where

import Data.List
import qualified Text.ParserCombinators.Parsec as P

data RE
  = Null
  | Term Char
  | Seq RE RE
  | Alt RE RE
  | Rep RE
  | Plus RE
  | Opt RE
  deriving (Eq, Show)


-- Задача 1 -----------------------------------------
simplify :: RE -> RE
simplify re = case re of
  Null -> Null
  Term c -> Term c
  Seq re1 re2 -> Seq (simplify re1) (simplify re2)
  Alt re1 re2 -> Alt (simplify re1) (simplify re2)
  Rep re -> Rep (simplify re)
  Plus re -> Seq re2 (Rep re2) where re2 = simplify re
  Opt re -> Alt re2 Null where re2 = simplify re

-- Задача 2 -----------------------------------------
isTerminal :: Automation -> State -> Bool
isTerminal (_, sts, _) s = elem s sts

isEssential :: Automation -> State -> Bool
isEssential aut@(_, _, trs) s =
  isTerminal aut s
    || ( any
           (\(s1, _, l) -> s1 == s && l /= Eps)
           (filter (\(is, _, _) -> is == s) (trs))
       )

-- Задача 3 -----------------------------------------
transitionsFrom :: Automation -> State -> [Transition]
transitionsFrom (_, _, trs) s = filter (\(is, _, _) -> is == s) (trs)

-- Задача 4 -----------------------------------------
labels :: [Transition] -> [Label]
labels trs = nub [l | (_, _, l) <- trs, l /= Eps]

-- Задача 5 -----------------------------------------
acceptsDA :: Automation -> String -> Bool
acceptsDA aut@(is, _, trs) wd = any (isTerminal aut) (acceptsDA' aut wd [is])

acceptsDA' :: Automation -> String -> [State] -> [State]
acceptsDA' aut "" sts = sts
acceptsDA' aut (x : xs) sts = acceptsDA' aut xs (setStep aut sts (C x))

-- Задача 6 -----------------------------------------
stStep :: Automation -> State -> Label -> [State]
stStep (_, _, trs) st mc = nub [s2 | (s1, s2, l) <- trs, s1 == st, l == mc]

setStep :: Automation -> [State] -> Label -> [State]
setStep naut bs mc = nub (concat [stStep naut s mc | s <- bs])

merge :: [State] -> [State] -> [State]
merge xs ys = xs ++ (filter (\x -> not (elem x xs)) ys)

closure :: Automation -> [State] -> [State]
closure naut sts =
  let newSteps = setStep naut sts Eps
      merged = sort (merge (sts) (newSteps))
   in if sts == merged then sts else closure naut (merged)

-- Задача 7 -----------------------------------------
accepts :: Automation -> String -> Bool
accepts aut@(is, _, _) str = any (isTerminal aut) (accepts' aut (closure aut [is]) str)

accepts' :: Automation -> [State] -> String -> [State]
accepts' aut@(is, _, _) sts [] = sts
accepts' aut@(is, _, _) sts (x : xs) = accepts' aut (closure aut (setStep aut sts (C x))) xs

-- Задача 8 -----------------------------------------
makeNDA :: RE -> Automation
makeNDA re = (1, [2], sort transitionsAll)
  where
    (transitionsAll, _) = make (simplify re) 1 2 3

--  data RE = Null           | -- Нуль вираз
--                   Term Char  | -- Термінальний символ
--                    Seq RE RE | -- Послідовність
--                   Alt RE RE  | -- Альтернатива
--                    Rep RE       | -- Повторення (*)
--                    Plus RE      | -- Повторення (+)
--                   Opt RE       | -- Необов’язкове входження (?)

make :: RE -> Int -> Int -> Int -> ([Transition], Int)
-- Seq (Rep (Alt (Term 'a') (Term 'b'))) (Term 'c')
-- (1,[2],[(1,3,Eps),(1,5,Eps),(3,4,Eps),(4,2,C 'c'),(5,7,Eps),(5,9,Eps),(6,3,Eps),(6,5,Eps),(7,8,C 'a'),(8,6,Eps),(9,10,C 'b'),(10,6,Eps)])

-- (1,[2],[(1,5,Eps),(1,3,Eps),(6,5,Eps),(6,3,Eps),(5,7,Eps),(5,9,Eps),(7,8,C 'a'),(9,10,C 'b'),(8,6,Eps),(10,6,Eps),(4,2,C 'c'),(3,4,Eps)])
-- (1,[2],[(1,3,Eps),(1,5,Eps),(3,4,Eps),(4,2,C 'c'),(5,7,Eps),(5,9,Eps),(6,3,Eps),(6,5,Eps),(7,8,C 'a'),(8,6,Eps),(9,10,C 'b'),(10,6,Eps)])

make Null beg fin nxt = ([(beg, fin, Eps)], nxt)
-- Term 'a' | Term 'b' | Term 'c'
make (Term c) beg fin nxt = ([(beg, fin, C c)], nxt)
-- Seq (Rep) (Term 'c')                              --  (3,4,Eps)
make (Seq r1 r2) m n k = (mr1 ++ [(k, k + 1, Eps)] ++ mr2, nx2)
  where
    (mr1, nx1) = make r1 m k (k + 2)
    (mr2, nx2) = make r2 (k + 1) n nx1
make (Alt r1 r2) m n k = ([(m, k, Eps), (m, k + 2, Eps)] ++ mr1 ++ mr2 ++ [(k + 1, n, Eps), (k + 3, n, Eps)], nx2)
  where
    -- (Term 'a')
    -- (Term 'b')
    (mr1, nx1) = make r1 k (k + 1) (k + 4)
    (mr2, nx2) = make r2 (k + 2) (k + 3) nx1
make (Rep r1) m n k = ([(m, k, Eps), (m, n, Eps), (k + 1, k, Eps), (k + 1, n, Eps)] ++ mr1, nx1)
  where
    -- (Alt (Term 'a') (Term 'b'))
    (mr1, nx1) = make r1 k (k + 1) (k + 2)

--
-- Задача 9 -----------------------------------------
parseReg :: String -> Maybe RE
parseReg wd = case P.parse reParser "" wd of
  Left _ -> Nothing
  Right r -> Just r

--    < reg>    :: =  <rexpr>  ‘eos’

reParser :: P.Parser RE
reParser = rexpr <* P.eof

--
--    < rexpr> :: = <rterm> { '|'  <rterm>}

rexpr :: P.Parser RE
rexpr = do
  r1 <- rterm
  r2 <- P.many (P.char '|' *> rterm)
  return (foldl Alt r1 r2)

--    < rterm> :: =  <rfact>  {<rfact>}

rterm :: P.Parser RE
rterm = do
  r1 <- rfact
  r2 <- P.many rfact
  return (foldl Seq r1 r2)

--    < rfact>  :: =  <prime> {'*' | '+' | '?'}

rfact :: P.Parser RE
rfact = do
  r1 <- prime
  opt <- P.many (P.oneOf "*+?")
  return
    ( foldl
        ( \r c -> case c of
            '*' -> Rep r
            '+' -> Seq r (Rep r)
            '?' -> Opt r
        )
        r1
        opt
    )

--    < prime> ::=  <rsymb> | '(' <rexpr> ')'
prime :: P.Parser RE
prime = rsymb P.<|> (P.char '(' *> rexpr <* P.char ')')

--    < rsymb> :: =<довільний символ крім  ‘(‘, ‘)’, ‘|’, ‘*’, ‘+’, ‘?’>
rsymb :: P.Parser RE
rsymb = do
  c <- P.noneOf "()|*+?"
  return (Term c)


type State = Int
data Label = C Char | Eps deriving (Eq, Ord, Show)
type Transition = (State, State, Label)
type Automation = (State, [State], [Transition])
type MetaState = [State]
type MetaTransition = (MetaState, MetaState, Label)

-- Задача 10 -----------------------------------------

-- Будує  початковий метастан imx, список всіх метастанів mtx і список метапереходів mtrx
makeDA' :: Automation -> (MetaState, [MetaState], [MetaTransition])
makeDA' aut@(is,_,_) = undefined

-- eclose :: Automation -> MetaState -> MetaState
-- eclose aut ms = [s | s <- closure aut ms, isEssential aut s]

-- makeDAStep :: Automation -> ([MetaState],[MetaState],[MetaTransition])  -> ([MetaState],[MetaState],[MetaTransition]) 
-- makeDAStep aut  (mx, (mn:mnx),mtx)   = (mn:mtx, mnx++nmx, mtx++nmtx) 
--                                         where 
--                                           nmtx = 
--                                           ls = getLabels aut mn



-- спочатку використовує makeDA’ aut, а потім перекодує метастани цілими числами, будуючи заключний автомат.
makeDA :: Automation -> Automation
makeDA aut@(_, fsx, _) = undefined
-------------------------------------------------------
-- showRE - Функція може бути корисною при тестуванні
showRE :: RE -> String
showRE (Seq re re') = showRE re ++ showRE re'
showRE (Alt re re') = "(" ++ showRE re ++ "|" ++ showRE re' ++ ")"
showRE (Rep re) = showRE' re ++ "*"
showRE (Plus re) = showRE' re ++ "+"
showRE (Opt re) = showRE' re ++ "?"
showRE re = showRE' re

showRE' :: RE -> String
showRE' Null = ""
showRE' (Term c) = [c]
showRE' (Alt re re') = showRE (Alt re re')
showRE' re = "(" ++ showRE re ++ ")"

--------------------------------------------------------
-- Тестові приклади
reFigureS, re1S, re2S, re3S, re4S, re5S, re6S :: String
reFigureS = "(a|b)*c"
re1S = "(x|y)(1|2)"
re2S = "x'*"
re3S = "(ab|c)*"
re4S = "(a?)a"
re5S = "(ab)?d+"
re6S = "c?*"

reFigure, re1, re2, re3, re4, re5, re6 :: RE
reFigure = Seq (Rep (Alt (Term 'a') (Term 'b'))) (Term 'c')
re1 = Seq (Alt (Term 'x') (Term 'y')) (Alt (Term '1') (Term '2'))
re2 = Seq (Term 'x') (Rep (Term '\''))
re3 = Rep (Alt (Seq (Term 'a') (Term 'b')) (Term 'c'))
re4 = Seq (Opt (Term 'a')) (Term 'a')
re5 = Seq (Opt (Seq (Term 'a') (Term 'b'))) (Plus (Term 'd'))
re6 = Rep (Opt (Term 'c'))

ndaFigure, nda1, nda2, nda3, nda4, nda5, nda6, ndaTest :: Automation
daFigure, da1, da2, da3, da4, da5, da6 :: Automation
ndaFigure =
  ( 1,
    [2],
    [ (1, 3, Eps),
      (1, 5, Eps),
      (3, 4, Eps),
      (4, 2, C 'c'),
      (5, 7, Eps),
      (5, 9, Eps),
      (6, 3, Eps),
      (6, 5, Eps),
      (7, 8, C 'a'),
      (8, 6, Eps),
      (9, 10, C 'b'),
      (10, 6, Eps)
    ]
  )

daFigure =
  (1, [2], [(1, 1, C 'a'), (1, 1, C 'b'), (1, 2, C 'c')])

nda1 =
  ( 1,
    [2],
    [ (1, 5, Eps),
      (1, 7, Eps),
      (3, 4, Eps),
      (4, 9, Eps),
      (4, 11, Eps),
      (5, 6, C 'x'),
      (6, 3, Eps),
      (7, 8, C 'y'),
      (8, 3, Eps),
      (9, 10, C '1'),
      (10, 2, Eps),
      (11, 12, C '2'),
      (12, 2, Eps)
    ]
  )

dal = (1, [2], 
     [(1,3, C 'a'), (1,2,C 'd'), (3,4,C 'b'), (4,2,C 'd'), (2,2,C 'd')])

da1 =
  ( 1,
    [3],
    [(1, 2, C 'x'), (1, 2, C 'y'), (2, 3, C '1'), (2, 3, C '2')]
  )

nda2 =
  ( 1,
    [2],
    [ (1, 3, C 'x'),
      (3, 4, Eps),
      (4, 2, Eps),
      (4, 5, Eps),
      (5, 6, C '\''),
      (6, 2, Eps),
      (6, 5, Eps)
    ]
  )

da2 =
  ( 1,
    [2],
    [(1, 2, C 'x'), (2, 2, C '\'')]
  )

nda3 =
  ( 1,
    [2],
    [ (1, 2, Eps),
      (1, 3, Eps),
      (3, 5, Eps),
      (3, 7, Eps),
      (4, 2, Eps),
      (4, 3, Eps),
      (5, 9, C 'a'),
      (6, 4, Eps),
      (7, 8, C 'c'),
      (8, 4, Eps),
      (9, 10, Eps),
      (10, 6, C 'b')
    ]
  )

da3 =
  ( 1,
    [1],
    [(1, 1, C 'c'), (1, 2, C 'a'), (2, 1, C 'b')]
  )

nda4 =
  ( 1,
    [2],
    [ (1, 5, Eps),
      (1, 7, Eps),
      (3, 4, Eps),
      (4, 2, C 'a'),
      (5, 6, C 'a'),
      (6, 3, Eps),
      (7, 8, Eps),
      (8, 3, Eps)
    ]
  )

da4 = (1, [2, 3], [(1, 2, C 'a'), (2, 3, C 'a')])

nda5 =
  ( 1,
    [2],
    [ (1, 5, Eps),
      (1, 7, Eps),
      (3, 4, Eps),
      (4, 11, C 'd'),
      (5, 9, C 'a'),
      (6, 3, Eps),
      (7, 8, Eps),
      (8, 3, Eps),
      (9, 10, Eps),
      (10, 6, C 'b'),
      (11, 12, Eps),
      (12, 2, Eps),
      (12, 13, Eps),
      (13, 14, C 'd'),
      (14, 2, Eps),
      (14, 13, Eps)
    ]
  )

da5 =
  ( 1,
    [2],
    [ (1, 2, C 'd'),
      (1, 3, C 'a'),
      (2, 2, C 'd'),
      (3, 4, C 'b'),
      (4, 2, C 'd')
    ]
  )

nda6 =
  ( 1,
    [2],
    [ (1, 2, Eps),
      (1, 3, Eps),
      (3, 5, Eps),
      (5, 6, C 'c'),
      (6, 4, Eps),
      (4, 2, Eps),
      (3, 7, Eps),
      (7, 8, Eps),
      (8, 4, Eps),
      (4, 3, Eps)
    ]
  )

da6 = (1, [1], [(1, 1, C 'c')])

ndaTest =
  ( 1,
    [1],
    [ (1, 2, C 'a'),
      (1, 4, Eps),
      (1, 3, C 'b'),
      (2, 3, Eps),
      (3, 5, Eps),
      (3, 4, C 'a'),
      (4, 4, Eps),
      (4, 1, Eps),
      (5, 2, Eps),
      (5, 4, Eps)
    ]
  )