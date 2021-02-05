{-# OPTIONS_GHC -Wall #-}
module SimpleLang where
-- не забывайте про тесты (SimpleLang_Test.hs)

-- Язык Simple -- очень простой императивный язык.
-- В нём только один тип данных: целые числа.

data Expression =
    Var String                   -- Переменные
  | Val Int                      -- Целые константы
  | Op Expression Bop Expression -- Бинарные операции
  deriving (Show, Eq)

data Bop = 
    Plus     
  | Minus    
  | Times    
  | Divide   
  | Gt       -- >
  | Ge       -- >=
  | Lt       -- <
  | Le       -- <=
  | Eql      -- ==
  deriving (Show, Eq)

data Statement =
    -- присвоить переменной значение выражения
    Assign   String     Expression
    -- увеличить переменную на единицу
  | Incr     String
    -- ненулевые значения работают как истина в if, while и for
  | If       Expression Statement  Statement
  | While    Expression Statement
  | For      Statement  Expression Statement Statement
    -- как { ... } в C-подобных языках
  | Block [Statement]
    -- пустая инструкция
  | Skip
  deriving (Show, Eq)

-- примеры программ на этом языке в конце модуля

-- по состоянию можно получить значение каждой переменной
-- (в реальной программе скорее использовалось бы Data.Map.Map String Int)
type State = String -> Int

-- Задание 1 -----------------------------------------

-- в начальном состоянии все переменные имеют значение 0
empty :: State
empty = const 0

-- возвращает состояние, в котором переменная var имеет значение newVal, 
-- все остальные -- то же, что в state
extend :: State -> String -> Int -> State
extend state var newVal = \x -> if x == var then newVal else state x

-- Задание 2 -----------------------------------------

-- возвращает значение выражения expr при значениях переменных из state.
eval :: State -> Expression -> Int
eval state (Var var) = state var

eval _     (Val val) = val

eval state (Op a oper b) =
  let expr_a = eval state a
      expr_b = eval state b
  in  case oper of
        Plus   -> expr_a + expr_b
        Minus  -> expr_a - expr_b
        Times  -> expr_a * expr_b
        Divide -> expr_a `div` expr_b
        Gt     -> fromEnum $ expr_a > expr_b
        Ge     -> fromEnum $ expr_a >= expr_b
        Lt     -> fromEnum $ expr_a < expr_b
        Le     -> fromEnum $ expr_a <= expr_b
        Eql    -> fromEnum $ expr_a == expr_b

-- Задание 3 -----------------------------------------

-- Можно выразить Incr через Assign, For через While, Block через 
-- последовательное выполнение двух инструкций (; в C).
-- Следующий тип задаёт упрощённый набор инструкций (промежуточный язык Simpler).
data DietStatement = DAssign String Expression
                   | DIf Expression DietStatement DietStatement
                   | DWhile Expression DietStatement
                   | DSequence DietStatement DietStatement
                   | DSkip
                     deriving (Show, Eq)

-- упрощает программу Simple
desugar :: Statement -> DietStatement
desugar (Assign str expr) = DAssign str expr

desugar (Incr str       ) = DAssign str (Op (Var str) Plus (Val 1))

desugar (If expr true_state false_state) =
  DIf expr (desugar true_state) (desugar false_state)

desugar (While whileCond statement) = DWhile whileCond (desugar statement)

desugar Skip                        = DSkip

desugar (Block []      )            = DSkip
desugar (Block (x : xs))            = DSequence (desugar x) (desugar $ Block xs)

desugar (For forInit forCond forUpd statement) =
    DSequence (desugar forInit) (
      DWhile forCond (
        DSequence (desugar statement) (desugar forUpd)
      )
    )

-- Задание 4 -----------------------------------------

-- принимает начальное состояние и программу Simpler
-- и возвращает состояние после работы программы
runSimpler :: State -> DietStatement -> State
runSimpler state (DAssign var expr) = extend state var $ eval state expr

runSimpler state (DIf expr true_state false_state) = if eval state expr == 1
  then runSimpler state true_state
  else runSimpler state false_state

runSimpler state DSkip                   = state

runSimpler state (DWhile expr statement) = if eval state expr == 1
  then runSimpler new_state (DWhile expr statement)
  else state
  where new_state = runSimpler state statement

runSimpler state (DSequence stat1 stat2) = runSimpler new_state stat2
  where new_state = runSimpler state stat1

-- 
-- in s "A" ~?= 10

-- принимает начальное состояние и программу Simple
-- и возвращает состояние после работы программы
run :: State -> Statement -> State
run state statement = runSimpler state $ desugar statement

-- Программы -------------------------------------------

{- Вычисление факториала

   for (Out := 1; In > 0; In := In - 1) {
     Out := In * Out
   }
-}
factorial :: Statement
factorial = For (Assign "Out" (Val 1))
                (Op (Var "In") Gt (Val 0))
                (Assign "In" (Op (Var "In") Minus (Val 1)))
                (Assign "Out" (Op (Var "In") Times (Var "Out")))


{- Вычисление целой части квадратного корня

   B := 0;
   while (A >= B * B) {
     B++
   };
   B := B - 1
-}
squareRoot :: Statement
squareRoot = Block [ Assign "B" (Val 0)
                   , While (Op (Var "A") Ge (Op (Var "B") Times (Var "B")))
                       (Incr "B")
                   , Assign "B" (Op (Var "B") Minus (Val 1))
                   ]

{- Вычисление числа Фибоначчи

   F0 := 1;
   F1 := 1;
   if (In == 0) {
     Out := F0
   } else {
     if (In == 1) {
       Out := F1
     } else {
       for (C := 2; C <= In; C++) {
         T  := F0 + F1;
         F0 := F1;
         F1 := T;
         Out := T
       }
     }
   }
-}
fibonacci :: Statement
fibonacci = Block [ Assign "F0" (Val 1)
                  , Assign "F1" (Val 1)
                  , If (Op (Var "In") Eql (Val 0))
                       (Assign "Out" (Var "F0"))
                       (If (Op (Var "In") Eql (Val 1))
                           (Assign "Out" (Var "F1"))
                           (For (Assign "C" (Val 2))
                                (Op (Var "C") Le (Var "In"))
                                (Incr "C")
                                (Block
                                 [ Assign "T" (Op (Var "F0") Plus (Var "F1"))
                                 , Assign "F0" (Var "F1")
                                 , Assign "F1" (Var "T")
                                 , Assign "Out" (Var "T")
                                 ])
                           )
                       )
                  ]
