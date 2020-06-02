{-# OPTIONS_GHC -Wall #-}
module FunctorsMonads where

-- ���������� ������� ����������� ������� Functor � Monad,
-- ����� ��� �� ������ �� ������������ ����������

-- ������������ �� ����������� ������:
-- https://hackage.haskell.org/package/base-4.8.0.0/docs/Data-Functor.html
-- https://hackage.haskell.org/package/base-4.8.0.0/docs/Control-Applicative.html
-- https://hackage.haskell.org/package/base-4.8.0.0/docs/Control-Monad.html
-- ��������, ��� ��������� � ���������, ����� ����� ���������� ������;
-- ������������ ����� �� ������.

class Functor' f where
  (<$$>) :: (a -> b) -> f a -> f b
  
class Functor' f => Applicative' f where
  pure' :: a -> f a
  (<**>) :: f (a -> b) -> f a -> f b
  
-- ������� 1 -----------------------------------------

-- ���������� join' ����� >>== � ��������
class Applicative' m => Monad' m where
  (>>==) :: m a -> (a -> m b) -> m b
  m >>== k = join' (k <$$> m)
  join' :: m (m a) -> m a
  join' m = m >>== id
-- ������
instance Functor' Maybe where
  _ <$$> Nothing = Nothing
  f <$$> Just x = Just (f x)
instance Applicative' Maybe where
  pure' = Just
  Just f <**> Just x = Just (f x)
  _ <**> _ = Nothing
instance Monad' Maybe where
  -- ���������� ���� �� ���������� ���� �� ������� >>== � join'
  Nothing >>== _ = Nothing
  Just x >>== f = f x
  join' Nothing = Nothing
  join' (Just x) = x

-- ������� 2 -----------------------------------------

-- ���������� �������, ������ �����

-- ignoreF ���������� ��������� "��� �� �����", ��� �������� (��������, ��� �������: 
-- ��� �� �����, ��� Maybe: Nothing ��� Just), �� �������� ����������
ignoreF :: Functor' f => f a -> f ()
ignoreF  = (<$$>) (const ())

-- "���������" ������� �� ���� ���������� � �������, ���������� � ����������
liftA2' :: Applicative' f => (a -> b -> c) -> f a -> f b -> f c
liftA2' f a b = f <$$> a <**> b

-- ��������� ��� �������� � ������ � �������� �� ���������� � ���� ������
seqA :: Applicative' f => [f a] -> f [a]
seqA = foldr (liftA2' (:)) (pure' [])

-- ��������� �������, ������������ ��������, �� ���� ��������� ������, ��������� ��� ��������
-- � �������� ���������� � ������
traverseA :: Applicative' f => (a -> f b) -> [a] -> f [b]
traverseA = (seqA .) . map

-- ���������� ������������ �������
composeM :: Monad' m => (b -> m c) -> (a -> m b) -> (a -> m c)
composeM fb fa = \x -> (pure' x) >>== fa >>== fb


-- ������� 3 -----------------------------------------

-- ���������� ���������� ������� �����

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

instance Functor' ((->) t) where -- (->) a b -- �� �� �����, ��� a -> b
  (<$$>) = (.)
  
instance Applicative' ((->) t) where
  pure' = const
  (<**>) f g x = f x (g x)
  
instance Monad' ((->) t) where
  f >>== k = \x -> k (f x) x
