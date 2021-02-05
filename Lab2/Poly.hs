-- Не забудьте добавить тесты.

{-# OPTIONS_GHC -Wall #-}
module Poly where

-- Многочлены
-- a -- тип коэффициентов, список начинается со свободного члена.
-- Бонус: при решении следующих заданий подумайте, какие стали бы проще или
-- сложнее при обратном порядке коэффициентов (и добавьте комментарий).
newtype Poly a = P [a]

-- Задание 1 -----------------------------------------

-- Определите многочлен $x$.
x :: Num a => Poly a
x = P [0, 1]

-- Задание 2 -----------------------------------------

-- Функция, считающая значение многочлена в точке

applyPoly :: Num a => Poly a -> a -> a
applyPoly (P []) _  = 0
applyPoly (P (a_0 : as)) x_0 = a_0 + x_0 * (applyPoly (P as) x_0)
-- Задание 3 ----------------------------------------

-- Определите равенство многочленов
-- Заметьте, что многочлены с разными списками коэффициентов
-- могут быть равны! Подумайте, почему.
instance (Num a, Eq a) => Eq (Poly a) where
    (==) (P []) (P []) = True
    (==) (P []) (P ys) = all (== 0) ys
    (==) (P xs) (P []) = all (== 0) xs
    (==) (P (a:as)) (P (b:bs)) = (a == b) && ((P as) == (P bs))
 
-- Задание 4 -----------------------------------------

-- Определите перевод многочлена в строку. 
-- Это должна быть стандартная математическая запись, 
-- например: show (3 * x * x + 1) == "3 * x^2 + 1").
-- (* и + для многочленов можно будет использовать после задания 6.)
instance (Num a, Eq a, Show a) => Show (Poly a) where
    show (P p) = let ps = reverse $ filter ((/= 0) . fst) $ zip p ([0..] :: [Integer])
                     showPoly [] = ["0"]
                     showPoly (a:as) = showFst a : map (wrap) (as) where
                        showSign coef = if signum coef == 1
                                        then " + "
                                        else " - "

                        showPow (_, 0) = ""
                        showPow (_, 1) = "x"
                        showPow (_, n) = "x^" ++ show n

                        showVal (coef, n) isFirst = showVal' (abs coef) isFirst where
                            showVal' 1 True = case (signum coef, n) of
                                                (_, 0) -> show coef
                                                (-1, _) -> "-"
                                                _ -> ""
                            showVal' _ True = show coef ++ if n == 0 then "" else " * "
                            showVal' 1 False = if n == 0 then show $ abs coef else ""
                            showVal' coef' False = show coef' ++ if n == 0 then "" else " * "

                        showFst coefN = showVal coefN True ++ showPow coefN
                        wrap coefN = showSign (fst coefN) ++ showVal coefN False ++ showPow coefN
                  in concat $ showPoly ps

-- Задание 5 -----------------------------------------

-- Определите сложение многочленов
plus :: Num a => Poly a -> Poly a -> Poly a
plus (P xs) (P ys) = P $ plus_1 xs ys
 where
  plus_1 [] b = b
  plus_1 a [] = a
  plus_1 (a : as) (b : bs) = (a + b) : plus_1 as bs

-- Задание 6 -----------------------------------------

-- Определите умножение многочленов
times :: Num a => Poly a -> Poly a -> Poly a
times (P xs) (P ys) = times_1 (P xs) (P ys)
 where
  times_1 (P []      ) _      = P []
  times_1 (P (a : as)) (P bs) = P (map (a *) bs) + times_1 (P as) (P (0 : bs))


-- Задание 7 -----------------------------------------

-- Сделайте многочлены числовым типом
instance Num a => Num (Poly a) where
    (+) = plus
    (*) = times
    negate (P p) = P $ map negate p
    fromInteger s = P [fromIntegral s]
    -- Эти функции оставить как undefined, поскольку для 
    -- многочленов они не имеют математического смысла
    abs    = undefined
    signum = undefined

-- Задание 8 -----------------------------------------

-- Реализуйте nderiv через deriv
class Num a => Differentiable a where
    -- взятие производной
    deriv  :: a -> a
    -- взятие n-ной производной
    nderiv :: Int -> a -> a
    nderiv n p = iterate deriv p !! n

-- Задание 9 -----------------------------------------

-- Определите экземпляр класса типов
instance Num a => Differentiable (Poly a) where
    deriv (P []) = P []
    deriv (P (_:ps)) = P $ deriv_1 ps 1 where
        deriv_1 [] _ = []
        deriv_1 (a:as) n = a * n : deriv_1 as (n + 1)
