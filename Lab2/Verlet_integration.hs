{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}

module Verlet_integration where

class Point t where
    infixl 6 ?+
    (?+) :: t -> t -> t
    infixl 7 ?*
    (?-) :: t -> t -> t
    (?*) :: Double -> t -> t
    infixl 6 ?-
    (?-) x y = x ?+ ((-1.0) ?* y)
    point_max :: t -> Double

instance Point Double where
    x ?+ y = x + y
    x ?* y = x * y
    point_max = abs

instance Point (Double, Double) where
    (x1, y1) ?+ (x2, y2) = (x1 + x2, y1 + y2)
    c ?* (x, y) = (c * x, c * y)
    point_max (x, y) = max (abs x) (abs y)

instance Point [Double] where
    xs ?+ ys = zipWith (+) xs ys
    x ?* ys = map (*x) ys
    point_max xs = maximum $ map abs xs

verlet_integration :: Point t => (t -> t) -> t -> t -> Double -> [t]
verlet_integration fA x0 v0 dt =
    let x1 = x0 ?+ dt ?* v0 ?+ (0.5 * dt * dt) ?*(fA x0)
        verlet_1 xPrev xCurr =
                let xNext = 2.0 ?* xCurr ?- xPrev ?+ (dt * dt) ?* (fA xCurr)
                in  xCurr : verlet_1 xCurr xNext
    in  x0 : verlet_1 x0 x1
