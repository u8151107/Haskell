{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE NoMonadFailDesugaring #-}
module SimpleLang1 where

import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Maybe
import qualified Data.Map as Map

data Expression =
    Var String                   -- Переменные
  | Lit Integer                  -- Целые константы
  | Op Expression Bop Expression -- Бинарные операции
  | Lambda String Expression     -- Лямбда-выражение
  | Ap Expression Expression     -- Вызов функции
  deriving (Show, Read, Eq)

data Value = IntVal Integer | FunVal Env String Expression
  deriving (Show, Read, Eq)

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
  deriving (Show, Read, Eq)

type Env = Map.Map String Value

-- в начальном состоянии переменных нет
empty :: Env
empty = Map.empty

-- возвращает состояние, в котором переменная var имеет значение newVal, 
-- все остальные -- то же, что в state
extend :: Env -> String -> Value -> Env
extend env var newVal = Map.insert var newVal env

-- 12 + (\x -> x) (4 + 2)
exampleExp1, exampleExp2 :: Expression
exampleExp1 = Op
  (Lit 12)
  Plus 
  (Ap
    (Lambda "x" (Var "x"))
    (Op (Lit 4) Plus (Lit 2)))

-- Выделено, чтобы можно было использовать во всех заданиях
opFun :: Bop -> Integer -> Integer -> Integer
opFun op = case op of
  Plus -> (+)
  Minus -> (-)
  Times -> (*)
  Divide -> div
  Gt -> boolFun (>)
  Ge -> boolFun (>=)
  Lt -> boolFun (<)
  Le -> boolFun (<=)
  Eql -> boolFun (==)
  where boolFun f x y = if (f x y) then 1 else 0

-- Базовый вариант интерпретатора
type Eval1 a = Identity a
runEval1 :: Eval1 a -> a
runEval1 = runIdentity

eval1 :: Env -> Expression -> Eval1 Value
eval1 env (Var name) = return $ fromJust (Map.lookup name env)
eval1 _   (Lit i) = return $ IntVal i
eval1 env (Op e1 op e2) = do IntVal i1 <- eval1 env e1
                             IntVal i2 <- eval1 env e2
                             return $ IntVal $ opFun op i1 i2
eval1 env (Lambda name body) = return $ FunVal env name body
eval1 env (Ap e1 e2) = do FunVal env' name body <- eval1 env e1
                          val2 <- eval1 env e2
                          eval1 (extend env' name val2) body

-- runEval1 (eval1 empty exampleExp1) == IntVal 18

-- Документация для используемых ниже типов *T: http://hackage.haskell.org/package/mtl-2.1.3.1
-- (У вас может быть другая версия библиотеки, эта включена в Haskell Platform 2014)

type Eval2 a = ExceptT String Identity a
runEval2 :: Eval2 a -> Either String a
runEval2 ev = runIdentity (runExceptT ev)

-- Добавьте сообщения об ошибках в случаях:
-- 1. Попытки взять значение переменной, которая отсутствует в окружении
-- 2. Ошибки типов (один их аргументов Op -- функция или 
-- первый аргумент Ap -- число)
-- Используйте throwError
eval2 :: Env -> Expression -> Eval2 Value
eval2 env (Var name) = case (Map.lookup name env) of
                            Nothing -> throwError "Variable is not in environment"
                            Just x -> return x
eval2 _   (Lit i) = return $ IntVal i
eval2 env (Op e1 op e2) = do val1 <- eval2 env e1
                             val2 <- eval2 env e2
                             case (val1, val2) of
                                  (IntVal i1, IntVal i2) -> return $ IntVal $ opFun op i1 i2
                                  _ -> throwError "Type error in Op"
eval2 env (Lambda name body) = return $ FunVal env name body
eval2 env (Ap e1 e2) = do arg1 <- eval2 env e1
                          val2 <- eval2 env e2
                          case arg1 of
                               FunVal env' name body -> eval2 (extend env' name val2) body
                               _ -> throwError "Type error in Ap"
-- runEval2 (eval2 empty exampleExp1) == Right (IntVal 18)
-- поскольку при вычислении нет ошибок

-- 1 + (\x -> x)
exampleExp2 = Op (Lit 1) Plus (Lambda "x" (Var "x"))
-- runEval2 (eval2 empty exampleExp2) == Left "Type error"

type Eval3 a = ReaderT Env (ExceptT String Identity) a
runEval3 :: Env -> Eval3 a -> Either String a
runEval3 env ev = runIdentity (runExceptT (runReaderT ev env))

-- С помощью ReaderT избегаем явной передачи окружения
eval3 :: Expression -> Eval3 Value
eval3 expr = do env <- ask
                lift $ eval2 env expr

-- runEval3 empty (eval3 exampleExp1) == Right (IntVal 18)

data DietStatement = DAssign String Expression
                   | DIf Expression DietStatement DietStatement
                   | DWhile Expression DietStatement
                   | DSequence DietStatement DietStatement
                   | DSkip
                     deriving (Show, Eq, Read)

-- Программа состоит из набора инструкций (которые могут быть объединены
-- в одну с помощью DSequence) и названия переменной, значение которой
-- является результатом программы
data Program = Program DietStatement String

type Eval4 a = StateT Env (ExceptT String Identity) a
runEval4 :: Env -> Eval4 a -> Either String (a, Env)
runEval4 env ev = runIdentity (runExceptT (runStateT ev env))

eval4 :: Program -> Eval4 Value
eval4 (Program statement var) = do _ <- eval4' statement
                                   env <- get
                                   lift $ eval2 env (Var var)
                                   where
                                     eval4' :: DietStatement -> Eval4 Value
                                     eval4' (DAssign varName expr) = do
                                       env <- get
                                       val <- lift $ eval2 env expr
                                       put $ extend env varName val
                                       return val
                                     eval4' (DIf expr statement1 statement2) = do
                                       env <- get
                                       val <- lift $ eval2 env expr
                                       case val of
                                         IntVal res -> if res == 1
                                                       then eval4' statement1
                                                       else eval4' statement2
                                         _ -> lift $ throwError "Type error in DIf"
                                     eval4' (DWhile expr statement') = eval4' (DIf expr
                                                                              (DSequence statement'
                                                                                         (DWhile expr statement'))
                                                                              DSkip)
                                     eval4' (DSequence statement1 statement2) = do _ <- eval4' statement1
                                                                                   eval4' statement2
                                     eval4' DSkip = return (IntVal 0)


type Eval2' a = ExceptT String IO a

eval2' :: Env -> Expression -> Eval2' Value
eval2' env (Var name) = case (Map.lookup name env) of
                            Nothing -> throwError "Variable is not in environment"
                            Just x -> return x
eval2' _   (Lit i) = return $ IntVal i
eval2' env (Op e1 op e2) = do val1 <- eval2' env e1
                              val2 <- eval2' env e2
                              case (val1, val2) of
                                   (IntVal i1, IntVal i2) -> return $ IntVal $ opFun op i1 i2
                                   _ -> throwError "Type error in Op"
eval2' env (Lambda name body) = return $ FunVal env name body
eval2' env (Ap e1 e2) = do arg1 <- eval2' env e1
                           val2 <- eval2' env e2
                           case arg1 of
                                FunVal env' name body -> eval2' (extend env' name val2) body
                                _ -> throwError "Type error in Ap"

type Eval5 a = StateT Env (ExceptT String IO) a
runEval5 :: Env -> Eval5 a -> IO (Either String (a, Env))
runEval5 env ev = runExceptT (runStateT ev env)

-- Печатает все промежуточные шаги вычислений
-- (используйте liftIO :: IO a -> Eval5 a)
eval5 :: Program -> Eval5 Value
eval5 (Program statement var) = do _ <- eval5' statement
                                   env <- get
                                   lift $ eval2' env (Var var)
                                   where
                                     eval5' :: DietStatement -> Eval5 Value
                                     eval5' (DAssign varName expr) = do
                                       env <- get
                                       val <- lift $ eval2' env expr
                                       put $ extend env varName val
                                       liftIO $ print val
                                       return val
                                     eval5' (DIf expr statement1 statement2) = do
                                       env <- get
                                       val <- lift $ eval2' env expr
                                       case val of
                                         IntVal res -> if res == 1
                                                       then eval5' statement1
                                                       else eval5' statement2
                                         _ -> lift $ throwError "Type error in DIf"
                                     eval5' (DWhile expr statement') = eval5' (DIf expr
                                                                              (DSequence statement'
                                                                                         (DWhile expr statement'))
                                                                              DSkip)
                                     eval5' (DSequence statement1 statement2) = do _ <- eval5' statement1
                                                                                   eval5' statement2
                                     eval5' DSkip = return (IntVal 0)

factorial :: DietStatement
factorial = DSequence (DAssign "Out" (Lit 1))
                      (DWhile (Op (Var "In") Gt (Lit 0))
                              (DSequence (DAssign "Out" (Op (Var "In") Times (Var "Out")))
                                         (DAssign "In" (Op (Var "In") Minus (Lit 1)))))
      
-- runEval5 (extend empty "In" (IntVal 5)) (eval5 $ Program factorial "Out")

-- Читает выражения со стандартного ввода и печатает результат
-- (используйте последний eval*, который сделаете)
main :: IO (Either String (Value, Env))
main = do
  input <- getLine
  let program = read $ input :: DietStatement
  runEval5 empty (eval5 $ Program program "Out")