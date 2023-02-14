{-# OPTIONS_GHC -Wall #-}

module Karahaiev10 where

import Data.List

-- розглядаємо лише цілі дані: скаляри  і масиви
--------------------------------------------------------------------
type Id = String

data Value = I Int | A [(Int, Int)] deriving (Eq, Show)

data Op = Add | Minus | Mul | Less | Equal | Index deriving (Eq, Show)

data Exp
  = Const Int
  | Var Id
  | OpApp Op Exp Exp
  | Cond Exp Exp Exp
  | FunApp Id [Exp]
  deriving (Eq, Show)

data Stmt
  = Assign Id Exp
  | AssignA Id Exp Exp
  | If Exp Stmt Stmt
  | While Exp Stmt
  | Call Id [Exp]
  | Block [VarDef] [Stmt]
  deriving (Eq, Show)

data VarDef = Arr Id | Int Id deriving (Eq, Show)

type FunDef = (Id, ([VarDef], Exp))

-- функції повертають лише цілі скалярні дані, не використовують глобальні дані (чисті!!)
type ProcDef = (Id, ([VarDef], Stmt))

type Program = ([VarDef], [FunDef], [ProcDef])

type StateP = [(Id, Value)] -- стек даних

data Type = At | It deriving (Eq, Show)

type FunEnv = [(Id, [Type])]

type ProcEnv = [(Id, [Type])]

type VarEnv = [(Id, Type)]

-- Задача 1 ------------------------------------
updateValue :: Eq a => a -> b -> [(a, b)] -> [(a, b)]
updateValue a b abs = case findIndex (== a) (map fst abs) of
  Just i -> take i abs ++ [(a, b)] ++ drop (i + 1) abs
  Nothing -> abs ++ [(a, b)]

-- Задача 2 ------------------------------------
updateArray :: Value -> Value -> Value -> Value
updateArray (A ps) (I i) (I v) = A (updateValue i v ps)

-- Задача 3 ------------------------------------
applyOp :: Op -> Value -> Value -> Value
applyOp Add (I i) (I j) = I (i + j)
applyOp Minus (I i) (I j) = I (i - j)
applyOp Mul (I i) (I j) = I (i * j)
applyOp Less (I i) (I j) = I (if i < j then 1 else 0)
applyOp Equal (I i) (I j) = I (if i == j then 1 else 0)
applyOp Index (A ps) (I i) = case lookup i ps of
  Just v -> I v
  Nothing -> I 0
applyOp _ _ _ = error "applyOp"

-- Задача 4 ------------------------------------
evExp :: Exp -> [FunDef] -> StateP -> Value
evExp (Const i) _ _ = I i
evExp (Var x) _ sb = lookUp x sb
evExp (OpApp o e1 e2) df sb = applyOp o (evExp e1 df sb) (evExp e2 df sb)
evExp (Cond e1 e2 e3) df sb = if evExp e1 df sb == I 0 then evExp e3 df sb else evExp e2 df sb
evExp (FunApp nm exs) dfx st = case lookup nm dfx of
  Just (vs, e) -> evExp e dfx (zip (map getVarName vs) (evArgs exs dfx st))
  Nothing -> error "evExp"

evArgs :: [Exp] -> [FunDef] -> StateP -> [Value]
evArgs exs dfx st = map (\e -> evExp e dfx st) exs

getVarName :: VarDef -> Id
getVarName (Int x) = x
getVarName (Arr x) = x

-- Задача 5 ------------------------------------
evStmt :: Stmt -> [FunDef] -> [ProcDef] -> StateP -> StateP
evStmt (Assign x e) df dp st = updateValue x (evExp e df st) st
evStmt (AssignA x e1 e2) df dp st = updateValue x (updateArray (lookUp x st) (evExp e1 df st) (evExp e2 df st)) st
evStmt (If e s1 s2) df dp st = if evExp e df st == I 0 then evStmt s2 df dp st else evStmt s1 df dp st
evStmt (While e s) df dp st = if evExp e df st == I 0 then st else evStmt (While e s) df dp (evStmt s df dp st)
evStmt (Call nm exs) df dp st = case lookup nm dp of
  Just (vs, s) -> evStmt s df dp (zip (map getVarName vs) (evArgs exs df st))
  Nothing -> error "evStmt"
evStmt (Block vs ss) df dp st = foldl (\st s -> evStmt s df dp st) st ss

-- Задача 6 ------------------------------------
iswfExp :: Exp -> VarEnv -> FunEnv -> Maybe Type
iswfExp (Const _) _ _ = Just It
iswfExp (Var x) venv _ = lookup x venv
iswfExp (OpApp o e1 e2) venv fenv = case (iswfExp e1 venv fenv, iswfExp e2 venv fenv) of
  (Just It, Just It) -> case o of
    Add -> Just It
    Minus -> Just It
    Mul -> Just It
    Less -> Just It
    Equal -> Just It
    _ -> Nothing
  (Just At, Just It) -> case o of
    Index -> Just It
    _ -> Nothing
  _ -> Nothing
iswfExp (Cond e1 e2 e3) venv fenv = case (iswfExp e1 venv fenv, iswfExp e2 venv fenv, iswfExp e3 venv fenv) of
  (Just It, Just t1, Just t2) -> if t1 == t2 then Just t1 else Nothing
  _ -> Nothing
iswfExp (FunApp nm exs) venv fenv = case lookup nm fenv of
  Just ts ->
    if length ts == length exs
      then if all (\(t, e) -> iswfExp e venv fenv == Just t) (zip ts exs) then Just It else Nothing
      else Nothing
  Nothing -> Nothing

-- Задача 7 ------------------------------------
iswfStmt :: Stmt -> VarEnv -> FunEnv -> ProcEnv -> Bool
iswfStmt (Assign x e) venv fenv penv = case (lookup x venv, iswfExp e venv fenv) of
  (Just t1, Just t2) -> t1 == t2
  _ -> False
iswfStmt (AssignA x e1 e2) venv fenv penv = case (lookup x venv, iswfExp e1 venv fenv, iswfExp e2 venv fenv) of
  (Just At, Just It, Just It) -> True
  _ -> False
iswfStmt (If e s1 s2) venv fenv penv = case iswfExp e venv fenv of
  Just It -> iswfStmt s1 venv fenv penv && iswfStmt s2 venv fenv penv
  _ -> False
iswfStmt (While e s) venv fenv penv = case iswfExp e venv fenv of
  Just It -> iswfStmt s venv fenv penv
  _ -> False
iswfStmt (Call nm exs) venv fenv penv = case lookup nm penv of
  Just ts ->
    if length ts == length exs
      then if all (\(t, e) -> iswfExp e venv fenv == Just t) (zip ts exs) then True else False
      else False
  Nothing -> False
iswfStmt (Block vs ss) venv fenv penv =
  let nms = map getVarName vs
      addVenv = venv ++ (map toVarDef vs)
   in (nub nms == nms)
        && all (== True) [iswfStmt stm addVenv fenv penv | stm <- ss]

toVarDef :: VarDef -> (Id, Type)
toVarDef (Int x) = (x, It)
toVarDef (Arr x) = (x, At)

unVarDef :: (Id, Type) -> VarDef
unVarDef (x, It) = Int x
unVarDef (x, At) = Arr x

iswfVarDef :: VarDef -> VarEnv -> Bool
iswfVarDef (Int x) venv = case lookup x venv of
  Just _ -> False
  Nothing -> True

-- Задача 8 ------------------------------------
iswfFunDef :: FunDef -> FunEnv -> Bool
iswfFunDef (_, (vs, b)) fe = (nub nms == nms) && maybe False (== It) (iswfExp b varsEnv fe)
  where
    varsEnv = map toVarDef vs
    nms = map fst varsEnv

iswfProcDef :: ProcDef -> VarEnv -> FunEnv -> ProcEnv -> Bool
iswfProcDef (_, (vs, b)) venv fe penv = (nub nms == nms) && iswfStmt b varsEnv fe penv
  where
    varsEnv = venv ++ (map toVarDef vs)
    nms = map fst varsEnv

-- Задача 9 ------------------------------------
iswfProgram :: Program -> Bool
iswfProgram = undefined

--- Допоміжні функції -----------------------------
lookUp :: Eq a => a -> [(a, b)] -> b
-- Передумова: Пара з ключом a є в списку пар abx
lookUp a abx = maybe (error "lookUp") id (lookup a abx)

-- формує початкове значення змінної
initv :: VarDef -> (Id, Value)
initv (Arr v) = (v, A [])
initv (Int v) = (v, I 0)

-- Реалізація виконання програми
evProgram :: Program -> StateP
evProgram (dvx, dfx, dpx) =
  let sb = map initv dvx
      (_, s) = lookUp "main" dpx
   in evStmt s dfx dpx sb

--  iswfOp o ts - перевіряє коректність типів операндів ts
--     бінарної операції o і формує тип результату Just t або Nothing
iswfOp :: Op -> [Type] -> Maybe Type
iswfOp Add [It, It] = Just It
iswfOp Minus [It, It] = Just It
iswfOp Mul [It, It] = Just It
iswfOp Less [It, It] = Just It
iswfOp Equal [It, It] = Just It
iswfOp Index [At, It] = Just It
iswfOp _ _ = Nothing

--  iswfCond ts - перевіряє коректність  типів операндів ts
--     умовного виразу і формує тип результату Just t або Nothing
iswfCond :: [Type] -> Maybe Type
iswfCond [It, It, It] = Just It
iswfCond [It, At, At] = Just At
iswfCond _ = Nothing

-- iswfAssignA ts перевіряє коректність  типів операндів ts
--   операції присвоювання значення елементу масива
iswfAssignA :: [Type] -> Bool
iswfAssignA [At, It, It] = True
iswfAssignA _ = False

---- Дані для тестування  -----------------------
-- Стан для тестування
sampleState :: StateP
sampleState = [("x", I 5), ("y", I 2), ("a", A [(2, 3), (0, 4), (1, 2)])]

varEnv :: VarEnv
varEnv = [("x", It), ("y", It), ("a", At)]

-- Функція максимум двох чисел
-- func biggest(m,n)= (m<n ? n : m)
biggest :: FunDef
biggest =
  ( "biggest",
    ( [Int "m", Int "n"],
      Cond (OpApp Less (Var "m") (Var "n")) (Var "n") (Var "m")
    )
  )

-- Функція, що обчислює число Фібоначчі
-- func fib(n) = (n<3 ? 1 : fib(n-1) + fib(n-2))
fib :: FunDef
fib =
  ( "fib",
    ( [Int "n"],
      Cond
        (OpApp Less (Var "n") (Const 3))
        (Const 1)
        ( OpApp
            Add
            (FunApp "fib" [OpApp Minus (Var "n") (Const 1)])
            (FunApp "fib" [OpApp Minus (Var "n") (Const 2)])
        )
    )
  )

-- Функція - сума елементів масиву 0..n ...
-- func sumA(a[],n) = (n<0 ? 0 : a[n] + sumA (a,n-1))
sumA :: FunDef
sumA =
  ( "sumA",
    ( [Arr "a", Int "n"],
      Cond
        (OpApp Less (Var "n") (Const 0))
        (Const 0)
        ( OpApp
            Add
            (OpApp Index (Var "a") (Var "n"))
            (FunApp "sumA" [Var "a", OpApp Minus (Var "n") (Const 1)])
        )
    )
  )

funEnv :: FunEnv
funEnv = [("biggest", [It, It]), ("fib", [It]), ("sumA", [At, It])]

-- Приклад оператору - блоку
sampleBlock :: Stmt
sampleBlock =
  Block
    [Arr "b"]
    [ AssignA "b" (Const 0) (Const 9),
      AssignA "b" (Const 2) (Const 5),
      AssignA "b" (Const 3) (Const 7),
      AssignA "b" (Const 5) (Const 1),
      Call "sumA1" [Var "b", Const 5]
    ]

-- Процедура - додавання двох чисел...
-- proc gAdd(x,y) gSum = x + y
gAdd :: ProcDef
gAdd =
  ( "gAdd",
    ( [Int "x", Int "y"],
      Assign "gSum" (OpApp Add (Var "x") (Var "y"))
    )
  )

-- Процедура - сума елементів масиву 0..n ...
-- proc sumA1(a[],n) {i;limit;
--      sA=0; i=0; limit=n+1;
--      while (i<limit){sA=sA+a[i]; i=i+1}
--                   }
sumA1 :: ProcDef
sumA1 =
  ( "sumA1",
    ( [Arr "a", Int "n"],
      Block
        [Int "i", Int "limit"]
        [ Assign "sA" (Const 0),
          Assign "i" (Const 0),
          Assign "limit" (OpApp Add (Var "n") (Const 1)),
          While
            (OpApp Less (Var "i") (Var "limit"))
            ( Block
                []
                [ Assign
                    "sA"
                    ( OpApp
                        Add
                        (Var "sA")
                        (OpApp Index (Var "a") (Var "i"))
                    ),
                  Assign "i" (OpApp Add (Var "i") (Const 1))
                ]
            )
        ]
    )
  )

procEnv :: ProcEnv
procEnv = [("gAdd", [It, It]), ("sumA1", [At, It])]

-- Повні програми
-- gSum;
-- proc gAdd(x,y) gSum = x + y
-- proc main() call gAdd(5,10)
pr1 :: Program
pr1 = ([Int "gSum"], [], [gAdd, ("main", ([], Call "gAdd" [Const 5, Const 10]))])

-- sA
-- proc sumA1(a[],n) {i;limit; .... }
-- proc main() {b[]; b[0]=9; b[2]=5; b[3]=7; b[5]=1;
--                   call sumA1 (b,5)
--             }

pr2 :: Program
pr2 =
  ( [Int "sA"],
    [],
    [ sumA1,
      ("main", ([], sampleBlock))
    ]
  )