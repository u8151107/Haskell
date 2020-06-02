{-# OPTIONS_GHC -Wall #-}
module FunctorsMonads where

-- определяем аналоги стандартных классов Functor и Monad,
-- чтобы нам не мешали их существующие экземпляры

-- документация на стандартные версии:
-- https://hackage.haskell.org/package/base-4.8.0.0/docs/Data-Functor.html
-- https://hackage.haskell.org/package/base-4.8.0.0/docs/Control-Applicative.html
-- https://hackage.haskell.org/package/base-4.8.0.0/docs/Control-Monad.html
-- заметьте, что посмотрев в источники, можно взять реализацию оттуда;
-- постарайтесь этого не делать.

class Functor' f where
  (<$$>) :: (a -> b) -> f a -> f b
  
class Functor' f => Applicative' f where
  pure' :: a -> f a
  (<**>) :: f (a -> b) -> f a -> f b
  
-- Задание 1 -----------------------------------------

-- реализуйте join' через >>== и наоборот
class Applicative' m => Monad' m where
  (>>==) :: m a -> (a -> m b) -> m b
  m >>== k = join' (k <$$> m)
  join' :: m (m a) -> m a
  join' m = m >>== id
-- пример
instance Functor' Maybe where
  _ <$$> Nothing = Nothing
  f <$$> Just x = Just (f x)
instance Applicative' Maybe where
  pure' = Just
  Just f <**> Just x = Just (f x)
  _ <**> _ = Nothing
instance Monad' Maybe where
  -- достаточно было бы определить одну из функций >>== и join'
  Nothing >>== _ = Nothing
  Just x >>== f = f x
  join' Nothing = Nothing
  join' (Just x) = x

-- Задание 2 -----------------------------------------

-- реализуйте функции, следуя типам

-- ignoreF возвращает результат "той же формы", что аргумент (например, для списков: 
-- той же длины, для Maybe: Nothing или Just), но забывает содержимое
ignoreF :: Functor' f => f a -> f ()
ignoreF  = (<$$>) (const ())

-- "Поднимает" функцию от двух аргументов в функцию, работающую с действиями
liftA2' :: Applicative' f => (a -> b -> c) -> f a -> f b -> f c
liftA2' f a b = f <$$> a <**> b

-- выполняет все действия в списке и собирает их результаты в один список
seqA :: Applicative' f => [f a] -> f [a]
seqA = foldr (liftA2' (:)) (pure' [])

-- применяет функцию, возвращающую действия, ко всем элементам списка, выполняет эти действия
-- и собирает результаты в список
traverseA :: Applicative' f => (a -> f b) -> [a] -> f [b]
traverseA = (seqA .) . map

-- композиция монадических функций
composeM :: Monad' m => (b -> m c) -> (a -> m b) -> (a -> m c)
composeM fb fa = \x -> (pure' x) >>== fa >>== fb


-- Задание 3 -----------------------------------------

-- реализуйте экземпляры классов типов

instance Functor' (Either t) where
  _ <$$> Left x = Left x
  f <$$> Right x = Right (f x)

instance Applicative' (Either t) where
  pure' = Right
  Left f <**> _ = Left f
  Right f <**> r = f <$$> r

instance Monad' (Either t) where
  Left x >>== _ = Left x
  Right x >>== f = f x

instance Functor' ((->) t) where -- (->) a b -- то же самое, что a -> b
  (<$$>) = (.)
  
instance Applicative' ((->) t) where
  pure' = const
  (<**>) f g x = f x (g x)
  
instance Monad' ((->) t) where
  f >>== k = \x -> k (f x) x
