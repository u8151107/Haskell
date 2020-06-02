{-# OPTIONS_GHC -Wall #-}
module Streams where

import Data.List(intercalate)
import Control.Applicative

-- ������� 1 -----------------------------------------

-- ��� Stream a ������������ ����������� ������ (������) �������� ���� a
-- (� ������� �� [a], ������� ����� ���� ��� ���������, ��� � ������������
data Stream a = a :> Stream a

-- ��������� Show ��� Stream a �������� ������ 10 ��������� ������
instance Show a => Show (Stream a) where
    show s = "[" ++ intercalate ", " (map show $ take 10 $ streamToList s)
             ++ ", ..."

-- ���������� �������, ������������ ����� � (�����������) ������
streamToList :: Stream a -> [a]
streamToList (a :> as) = a : streamToList as

-- ������� 2 -----------------------------------------

-- ���������� ��������� ������� ������� ��� ������ � ��������

-- �����, ��������� �� ���������� ���������
sRepeat :: a -> Stream a
sRepeat b = c where
    c = b :> c

-- sRepeat 1 == [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, ...

-- �����, ��������� �� ������������ ����� �������� ������
-- (���������: ��� � ���������� ����� ����������� ���, ��� ���������� ����� 
-- ����� ����������� (��������� ��� �� ����), � �� ���������� ��������)
sCycle :: [a] -> Stream a
sCycle bs = cs where
    cs = foldr (:>) cs bs

-- sCycle [1, 2, 3] == [1, 2, 3, 1, 2, 3, 1, 2, 3, 1, ...

-- �����, �������� ��������� ��������� � ��������, �������� ��������� ��������
-- �� ��������
sIterate :: (a -> a) -> a -> Stream a
sIterate fun b = b :> sIterate fun (fun b)

-- sIterate (/ 2) 1.0 == [1.0, 0.5, 0.25, 0.125, 0.0625, ...

-- �������, ������������ n ������ ��������� ������
sTake :: Int -> Stream a -> [a]
sTake n (b :> bs) | n == 0 = []
                  | n > 0 = b : sTake (n - 1) bs
                  | otherwise = error ("n < 0")

-- sTake 3 $ sRepeat 1 == [1, 1, 1]

-- �������, ������������ ����� �� ������������ ��������� ���� �������
-- (��� ���������� ������� ����� ������� ��� ������� ������� ��
-- ������� ���������, �� ���� �� ������������ ��� � ��������)
sInterleave :: Stream a -> Stream a -> Stream a
sInterleave (b :> bs) cs = b :> sInterleave cs bs

-- sInterleave (sRepeat 1) (sRepeat 2) == [1, 2, 1, 2, 1, 2, ...

-- ������� 3 -----------------------------------------

-- ��������� ���������� �������, ����������

-- ����� ����������� ����� (������� � 1)
nats :: Stream Integer
nats = sIterate (+ 1) 1

-- nats == [1, 2, 3, 4, 5, 6, 7, ...

-- �����, n-��� ������� �������� (������� � 1) -- ������������ ������� 2,
-- ������� n ������. ���������: � ������� sInterleave ��� ����� ������� ��� 
-- �������� �� ���������, ���� � ���������� ������ �� ������� ���������
-- (���������, ������ ��� �����).
ruler :: Stream Integer
ruler = hel 0 where
        hel n = sInterleave (sRepeat n) (hel (n + 1))

-- ruler == [0, 1, 0, 2, 0, 1, 0, 3, ...

-- ������� 4 -----------------------------------------

-- ���������� �������� ������������ ��������� ������-��������� �����.
-- ��� ������ �������� x ��������� ���������� �� �������
-- x' = (1103515245 * x + 12345) `mod` 2^31
-- (��� ��������� ������������, ��������, � GCC).
-- ��������� � ���������: https://ru.wikipedia.org/wiki/%D0%9B%D0%B8%D0%BD%D0%B5%D0%B9%D0%BD%D1%8B%D0%B9_%D0%BA%D0%BE%D0%BD%D0%B3%D1%80%D1%83%D1%8D%D0%BD%D1%82%D0%BD%D1%8B%D0%B9_%D0%BC%D0%B5%D1%82%D0%BE%D0%B4
rand :: Int -> Stream Int
rand = sIterate (\x -> (1103515245 * x + 12345) `mod` 2^(31 ::Integer))


-- ������� 5 -----------------------------------------

minMaxSlow, minMax, minMaxBang :: [Int] -> Maybe (Int, Int)
{- Total time: 107s Total Memory in use: 39 -}
minMaxSlow [] = Nothing
minMaxSlow xs = Just (minimum xs, maximum xs)

-- ������� minMax ������ �������� ����������� � ������������ �������� ������,
-- ��� ��, ��� minMaxSlow. �������� � minMaxSlow � ���, ��� ��� �������� �� ������ ��� ����
-- � ������� ��������� ��������� ��� � ������ �������. ���������� minMax ���, ����� 
-- ������� ������ ���� ������ �� ������.

{- Total time: 0.611s Total Memory in use: ??? -}
minMax (x:xs) = hel_1 (x,x) xs where
    hel_1 p [] = Just p
    hel_1 (curMin,curMax) (y:ys) = hel_1 (min curMin y,max curMax y) ys

-- �������������� �������: ���������� �� �� ����� ������� (��� ��������� minMaxBang) �
-- �������������� ����� ��������� (seq �/��� !)

{- Total time: ??? Total Memory in use: 189 -}
minMaxBang = undefined

-- ������������� ��������� � ����������� `ghc Streams.hs -O -rtsopts -main-is Streams`
-- � ��������� `Streams.exe +RTS -s` (./Streams � Linux/OSX).
-- ������������ ������ ����� � https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/runtime-control.html.
-- �������� ����� ���������� � ����� ������ ��� ������ ���������.
-- ����� ���������� �� ������ ��� ���������� ����������� (-O)

main :: IO ()
main = print $ minMaxSlow $ sTake 1000000 $ rand 7666532
-- main = print $ minMax $ sTake 1000000 $ rand 7666532
-- main = print $ minMaxBang $ sTake 1000000 $ rand 7666532


-- ������� 6 (������ ����� ��� ������ � FunctorsMonads) ----------------------------------

-- ���������� ���������� ������� ��� �������
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
			