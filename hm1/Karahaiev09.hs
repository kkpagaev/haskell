{-# OPTIONS_GHC -Wall #-}

module Karahaiev09 where

import Data.Char (digitToInt, isDigit)
import Text.ParserCombinators.Parsec

data Term
  = Nmb Int -- десяткове число без знаку
  | Var String -- змінна, довільний ідентифікатор
  | App Term Term -- операція застосування
  | Abs String Term --  операція абстракції
  deriving (Show, Eq)

type Contex = [(String, Term)]

-- Задача 1.a -----------------------------------------
addVar :: String -> [String] -> [String]
addVar v vs = if elem v vs then vs else v : vs

-- Задача 1.b -----------------------------------------
delVar :: String -> [String] -> [String]
delVar v vs = filter (/= v) vs

-- Задача 1.c -----------------------------------------
unionV :: [String] -> [String] -> [String]
unionV vs1 vs2 = foldr addVar vs1 vs2

-- Задача 1.d -----------------------------------------
freeVars :: Term -> [String]
freeVars t = case t of
  Nmb _ -> []
  Var v -> [v]
  App t1 t2 -> unionV (freeVars t1) (freeVars t2)
  Abs v t -> delVar v (freeVars t)

-- Задача 2.a -----------------------------------------
deleteSyn :: String -> Contex -> Contex
deleteSyn nm ctx = filter (\(x, _) -> x /= nm) ctx

-- Задача 2.b -----------------------------------------
iswfTerm :: Term -> Contex -> Bool
iswfTerm t ctx =
  let free = freeVars t
      inds = map (\(x, _) -> x) ctx
   in all (\x -> elem x inds || (not (null (x)) && all isDigit x)) free || null free

-- Задача 2.c -----------------------------------------
iswfContex :: Contex -> Bool
iswfContex ctx = iswfContex' ctx []

type Scope = [String]

iswfContex' :: Contex -> Scope -> Bool
iswfContex' [] _ = True
iswfContex' ((x, t) : ctx) scope =
  let free = freeVars t
   in all (\x -> elem x scope || (not (null (x)) && all isDigit x)) free && iswfContex' ctx (x : scope)

-- Задача 3.a -----------------------------------------
isNumber :: Term -> Bool
isNumber (Abs s (Abs z t)) = isNumber' (s, z) t
isNumber _ = False

isNumber' :: (String, String) -> Term -> Bool
isNumber' (s, z) (App (Var s') t) = s == s' && isNumber' (s, z) t
isNumber' (_, z) (Var z') = z == z'
isNumber' _ _ = False

-- Задача 3.b -----------------------------------------
inNumber :: Term -> Term
inNumber (Abs s (Abs z t)) = if isNumber' (s, z) t then Nmb (inNumber' (s, z) t) else error "Not a number"
inNumber _ = error "Not a number"

inNumber' :: (String, String) -> Term -> Int
inNumber' (s, z) (App (Var s') t) = 1 + (inNumber' (s, z) t)
inNumber' (_, z) (Var z') = 0

-- Задача 3.c -----------------------------------------

compress :: Term -> Term
compress te1@(Abs s te2@(Abs z t)) = if isNumber te1 then inNumber te1 else Abs s (compress (te2))
compress (App t1 t2) = App (compress t1) (compress t2)
compress (Abs s t) = Abs s (compress t)
compress t = t

-- Задача 4 -----------------------------------------
reduce :: Term -> String -> Term -> Term
reduce (App t1 t2) s w = App (reduce t1 s w) (reduce t2 s w)
reduce t@(Abs so t2) s w =
  let free = freeVars t
      inc = incVar so
   in if elem s free then Abs (inc) (reduce (reduce t2 so (Var inc)) s w) else Abs so (reduce t2 s w)
reduce t s w =
  let free = freeVars t
   in if elem s free then w else t

incVar :: String -> String
incVar str@(s : st) = if isDigit s then ((show ((digitToInt s) + 1)) ++ st) else ("0" ++ str)

-- Задача 5 -----------------------------------------
evalStep :: Term -> Contex -> Maybe Term
evalStep (Nmb t) _ = Just (integerTerm t)
evalStep (Var s) ctx = case lookup' (Var s) ctx of
  Just t -> Just t
  Nothing -> Nothing
evalStep (App (Abs s t) t2) ctx = Just (reduce (Abs s t) s t2)
evalStep (App t1 t2) ctx = case evalStep t1 ctx of
  Just t -> Just (App t t2)
  Nothing -> case evalStep t2 ctx of
    Just t -> Just (App t1 t)
    Nothing -> Nothing
evalStep (Abs s t) ctx = case evalStep t ctx of
  Just t' -> Just (Abs s t')
  Nothing -> Nothing

lookup' :: Term -> Contex -> Maybe Term
lookup' _ [] = Nothing
lookup' v ((x, t) : ctx) = if v == (Var x) then Just t else lookup' v ctx

-- Задача 6 -----------------------------------------
eval :: Int -> Term -> Contex -> Maybe Term
eval st t ctx = eval' st (Just t) ctx

eval' :: Int -> Maybe Term -> Contex -> Maybe Term
eval' _ Nothing _ = Nothing
eval' 0 t _ = t
eval' i t ctx = case t of
  Just t' -> eval' (i - 1) (evalStep t' ctx) ctx
  Nothing -> Nothing

-- Задача 7 -----------------------------------------
parseTerm :: String -> Maybe Term
parseTerm str = case parse expr "" str of
  (Left _) -> Nothing
  (Right x) -> Just x

expr :: Parser Term
expr = do _ <- spaces; v <- term; eof; return v

term :: Parser Term
term = do
  fss <- many1 factor
  return
    ( if length fss == 1
        then head fss
        else
          let (f : fs) = fss
           in foldl App f fs
    )

factor :: Parser Term
factor = do
  _ <- spaces
  v <- var <|> func <|> nmb <|> par
  _ <- spaces
  return v

var :: Parser Term
var = do
  v <- many1 letter
  return (Var v)

param :: Parser String
param = do
  _ <- spaces
  v <- many1 letter
  _ <- spaces
  return (v)

func :: Parser Term
func = do
  _ <- char '\\'
  ps <- many1 param
  _ <- char '.'
  t <- term
  return (foldl (flip Abs) t (reverse ps))

nmb :: Parser Term
nmb = do
  v <- many1 digit
  return (Nmb (read v))

par :: Parser Term
par = do
  _ <- char '('
  t <- term
  _ <- char ')'
  return t

--------------------------------------------------------
-- integerTerm - з числа в вираз
integerTerm :: Int -> Term
integerTerm n = (Abs "s" (Abs "z" (buildTerm n)))
  where
    buildTerm 0 = Var "z"
    buildTerm j = (App (Var "s") (buildTerm (j - 1)))

--  New Name -- якщо імя dddname, де ddd-цифри і n-буква, то початкове імя - name
-- якщо змінна ccc, то її нові імена 0ccc,...,9ccc,09ccc,...
-- цифри на початку - це створення нового імені (problem name capture)
newVar :: [String] -> String -> String
newVar fvs nm = (until (\n -> notElem n fvs) next) (next nm) -- flip elem fvs
  where
    next n@(c : _) | c == '9' = '0' : n
    next (c : cx) | isDigit c = (succ c) : cx
    next n = '0' : n

--------------------------------------------------------
-- Тестові приклади
term0, term0a, term1, term1a, term1b, term1c :: Term
term0 = Abs "s" (Abs "z" (App (Var "s") (App (Var "s") (Var "z"))))
term0a = Abs "z" (App (Var "s") (App (Var "s") (Var "z")))
term1 = Abs "y" (App (Abs "x" (Abs "y" (App (Var "x") (Var "y")))) (Var "y"))
term1a = App (Abs "x" (Abs "y" (App (Var "x") (Var "y")))) (Var "y")
term1b = Abs "x" (Abs "y" (App (Var "x") (Var "y")))
term1c = Abs "y" (App (Var "x") (Var "y"))

term2, term2a, termAnd, termTest :: Term
term2 =
  App
    ( App
        (Abs "f" (Abs "g" (Abs "x" (App (App (Var "f") (Var "x")) (App (Var "g") (Var "x"))))))
        (Abs "x" (Abs "y" (Var "x")))
    )
    (Abs "x" (Abs "y" (Var "x")))
term2a = App (Var "x") (App (Abs "x" (Var "x")) (App (Abs "x" (Var "x")) (Var "z")))
termAnd = Abs "x" (Abs "y" (App (App (Var "x") (Var "y")) (Var "false")))
termTest = Abs "x" (Abs "x" (Abs "y" (Var "y")))

cont1 :: Contex
cont1 =
  [ ("true", Abs "x" (Abs "y" (Var "x"))),
    ("false", Abs "x" (Abs "y" (Var "y"))),
    ("test", Abs "l" (Abs "m" (Abs "n" (App (App (Var "l") (Var "m")) (Var "n"))))),
    ("iszero", Abs "n" (App (App (Var "n") (Abs "x" (Var "false"))) (Var "true"))),
    ("plus", Abs "m" (Abs "n" (Abs "f" (Abs "x" (App (App (Var "n") (Var "f")) (App (App (Var "m") (Var "f")) (Var "x"))))))),
    ("mult", Abs "m" (Abs "n" (Abs "f" (App (Var "m") (App (Var "n") (Var "f")))))),
    ("pred", Abs "n" (Abs "f" (Abs "x" (App (App (App (Var "n") (Abs "g" (Abs "h" (App (Var "h") (App (Var "g") (Var "f")))))) (Abs "u" (Var "x"))) (Abs "x" (Var "x")))))),
    ("fixM", Abs "f" (App (Abs "x" (App (Var "f") (Abs "y" (App (App (Var "x") (Var "x")) (Var "y"))))) (Abs "x" (App (Var "f") (Abs "y" (App (App (Var "x") (Var "x")) (Var "y"))))))),
    ("sumR", Abs "r" (Abs "n" (App (App (App (Var "test") (App (Var "iszero") (Var "n"))) (Nmb 0)) (App (App (Var "plus") (Var "n")) (App (Var "r") (App (Var "pred") (Var "n"))))))),
    ("factR", Abs "fact" (Abs "n" (App (App (App (Var "test") (App (Var "iszero") (Var "n"))) (Nmb 1)) (App (App (Var "mult") (Var "n")) (App (Var "fact") (App (Var "pred") (Var "n"))))))),
    ("sum", App (Var "fixM") (Var "sumR")),
    ("factor", App (Var "fixM") (Var "factR"))
  ]

termS2 :: String
termS2 = "(\\f g x. f x (g x)) (\\x y .x) (\\x y .x)"