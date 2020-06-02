{-# OPTIONS_GHC -Wall #-}
module Streams where

import Data.List(intercalate)
import Control.Applicative

-- Задание 1 -----------------------------------------

-- Тип Stream a представляет бесконечные списки (потоки) значений типа a
-- (в отличие от [a], которые могут быть как конечными, так и бесконечными
data Stream a = a :> Stream a

-- Экземпляр Show для Stream a печатает первые 10 элементов потока
instance Show a => Show (Stream a) where
    show s = "[" ++ intercalate ", " (map show $ take 10 $ streamToList s)
             ++ ", ..."

-- Реализуйте функцию, превращающую поток в (бесконечный) список
streamToList :: Stream a -> [a]
streamToList (a :> as) = a : streamToList as

-- Задание 2 -----------------------------------------

-- Реализуйте несколько простых функций для работы с потоками

-- поток, состоящий из одинаковых элементов
sRepeat :: a -> Stream a
sRepeat b = c where
    c = b :> c

-- sRepeat 1 == [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, ...

-- поток, состоящий из бесконечного числа повторов списка
-- (подсказка: эту и предыдущую можно реализовать так, что полученный поток 
-- будет циклическим (ссылаться сам на себя), а не бесконечно растущим)
sCycle :: [a] -> Stream a
sCycle bs = cs where
    cs = foldr (:>) cs bs

-- sCycle [1, 2, 3] == [1, 2, 3, 1, 2, 3, 1, 2, 3, 1, ...

-- поток, заданный начальным значением и функцией, строящей следующее значение
-- по текущему
sIterate :: (a -> a) -> a -> Stream a
sIterate fun b = b :> sIterate fun (fun b)

-- sIterate (/ 2) 1.0 == [1.0, 0.5, 0.25, 0.125, 0.0625, ...

-- функция, возвращающая n первых элементов потока
sTake :: Int -> Stream a -> [a]
sTake n (b :> bs) | n == 0 = []
                  | n > 0 = b : sTake (n - 1) bs
                  | otherwise = error ("n < 0")

-- sTake 3 $ sRepeat 1 == [1, 1, 1]

-- функция, возвращающая поток из чередующихся элементов двух потоков
-- (для следующего задания нужно сделать эту функцию ленивой по
-- второму аргументу, то есть не сопоставлять его с образцом)
sInterleave :: Stream a -> Stream a -> Stream a
sInterleave (b :> bs) cs = b :> sInterleave cs bs

-- sInterleave (sRepeat 1) (sRepeat 2) == [1, 2, 1, 2, 1, 2, ...

-- Задание 3 -----------------------------------------

-- Используя предыдущие функции, реализуйте

-- поток натуральных чисел (начиная с 1)
nats :: Stream Integer
nats = sIterate (+ 1) 1

-- nats == [1, 2, 3, 4, 5, 6, 7, ...

-- поток, n-ный элемент которого (начиная с 1) -- максимальная степень 2,
-- делящая n нацело. Подсказка: с помощью sInterleave это можно сделать без 
-- проверок на делимость, если её реализация ленива по второму аргументу
-- (подумайте, почему это важно).
ruler :: Stream Integer
ruler = hel 0 where
        hel n = sInterleave (sRepeat n) (hel (n + 1))

-- ruler == [0, 1, 0, 2, 0, 1, 0, 3, ...

-- Задание 4 -----------------------------------------

-- Реализуйте линейный конгруэнтный генератор псевдо-случайных чисел.
-- При данном значении x следующее получается по формуле
-- x' = (1103515245 * x + 12345) `mod` 2^31
-- (эти параметры используются, например, в GCC).
-- Подробнее в Википедии: https://ru.wikipedia.org/wiki/%D0%9B%D0%B8%D0%BD%D0%B5%D0%B9%D0%BD%D1%8B%D0%B9_%D0%BA%D0%BE%D0%BD%D0%B3%D1%80%D1%83%D1%8D%D0%BD%D1%82%D0%BD%D1%8B%D0%B9_%D0%BC%D0%B5%D1%82%D0%BE%D0%B4
rand :: Int -> Stream Int
rand = sIterate (\x -> (1103515245 * x + 12345) `mod` 2^(31 ::Integer))


-- Задание 5 -----------------------------------------

minMaxSlow, minMax, minMaxBang :: [Int] -> Maybe (Int, Int)
{- Total time: 107s Total Memory in use: 39 -}
minMaxSlow [] = Nothing
minMaxSlow xs = Just (minimum xs, maximum xs)

-- функция minMax должна находить минимальное и максимальное значение списка,
-- так же, как minMaxSlow. Проблема с minMaxSlow в том, что она проходит по списку два раза
-- и поэтому вынуждена сохранять его в памяти целиком. Реализуйте minMax так, чтобы 
-- сделать только один проход по списку.

{- Total time: 0.611s Total Memory in use: ??? -}
minMax (x:xs) = hel_1 (x,x) xs where
    hel_1 p [] = Just p
    hel_1 (curMin,curMax) (y:ys) = hel_1 (min curMin y,max curMax y) ys

-- Дополнительное задание: реализуйте ту же самую функцию (под названием minMaxBang) с
-- использованием явной строгости (seq и/или !)

{- Total time: ??? Total Memory in use: 189 -}
minMaxBang = undefined

-- Скомпилируйте программу с аргументами `ghc Streams.hs -O -rtsopts -main-is Streams`
-- и запустите `Streams.exe +RTS -s` (./Streams в Linux/OSX).
-- Документацию можете найти в https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/runtime-control.html.
-- Сравните время выполнения и общую память для разных вариантов.
-- Также посмотрите на эффект при отключении оптимизаций (-O)

main :: IO ()
main = print $ minMaxSlow $ sTake 1000000 $ rand 7666532
-- main = print $ minMax $ sTake 1000000 $ rand 7666532
-- main = print $ minMaxBang $ sTake 1000000 $ rand 7666532


-- Задание 6 (делать после или вместе с FunctorsMonads) ----------------------------------

-- Реализуйте экземпляры классов для потоков
instance Functor Stream where
    fmap b (a :> as) = b a :> fmap b as

instance Applicative Stream where
    pure = sRepeat
    (<*>) (b :> bs) (a :> as) = b a :> (bs <*> as)

instance Monad Stream where
    return = pure
    (>>=) funk f = f1 (fmap f funk) where
        f2 (x :> _) = x
        f3 (_ :> xs) = xs
        f1 (xs :> xss) = (f2 xs) :> (f1 (fmap f3 xss))
			