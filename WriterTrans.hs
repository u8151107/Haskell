module WriterTrans where
import Data.Monoid
import Control.Applicative
import Control.Monad.Identity
import Control.Monad.Trans.Class

-- Writer w a -- тип вычислений с возвращаемым значением типа a
-- которые могут собирать какую-то информацию по ходу вычисления
newtype Writer w a = Writer { runWriter :: (a, w) }

-- он является монадой: return x возвращает значение, не записывая ничего в лог
-- а w >>= f применяет функцию f к значению действия w и комбинирует логи
instance Functor (Writer w) where
  fmap f (Writer (x, v)) = Writer (f x, v)
instance (Monoid w) => Applicative (Writer w) where
  pure x = Writer (x, mempty)
  Writer (f, v1) <*> Writer (x, v2) = Writer (f x, v1 <> v2)
instance (Monoid w) => Monad (Writer w) where  
  return = pure
  Writer (x,v) >>= f = let Writer (y, v') = f x in Writer (y, v <> v')

-- Некоторые полезные функции для Writer
-- writer (x, v) -- действие cо значением x и логом v
writer :: (a, w) -> Writer w a
writer (x, v) = Writer (x, v)
-- tell v -- действие со значением () и логом v
tell :: w -> Writer w ()
tell v = Writer ((), v)
-- listen w -- добавляет лог к значению действия
listen :: Writer w a -> Writer w (a, w)
listen (Writer (x, v)) = Writer ((x, v), v)
execWriter :: Writer w a -> w
execWriter = snd . runWriter

-- Трансформер монад, соответствующий Writer
-- При правильной реализации
-- WriterT w Identity эквивалентен Writer w 
-- writer' действует так же, как writer, при условии m == Identity
-- и т.д.
newtype WriterT w m a = WriterT { runWriterT :: m (a, w) }

instance (Functor m) => Functor (WriterT w m) where
  fmap f m = WriterT $ fmap k (runWriterT m)
    where k (x, v) = (f x, v)
instance (Monoid w, Applicative m) => Applicative (WriterT w m) where
  pure a  = WriterT $ pure (a, mempty)
  m1 <*> m2 = WriterT $ liftA2 k (runWriterT m1) (runWriterT m2)
    where k (f, v1) (x, v2) = (f x, v1 <> v2)
instance (Monoid w, Monad m) => Monad (WriterT w m) where
  return = pure
  m >>= f  = WriterT $ do
    (x, v)  <- runWriterT m
    (y, v') <- runWriterT (f x)
    return (y, v <> v')
instance (Monoid w) => MonadTrans (WriterT w) where
  lift m = WriterT $ do
    a <- m
    return (a, mempty)

writer' :: Monad m => (a, w) -> WriterT w m a
writer' (x, v) = WriterT $ return (x, v)

tell' :: (Monad m, Monoid w) => w -> WriterT w m ()
tell' v = WriterT $ return ((), v)

listen' :: (Monad m, Monoid w) => WriterT w m a -> WriterT w m (a, w)
listen' mw = WriterT $ do
  (a, w) <- runWriterT mw
  return ((a, w), w)

execWriterT :: Monad m => WriterT w m a -> m w
execWriterT mw = do
  (_, w) <- runWriterT mw
  return w
  
-- Дополнительное задание: реализовать StateT

-- Трансформер монад, соответствующий []
-- _После того, как его реализуете,_ прочитайте
-- http://www.haskell.org/haskellwiki/ListT_done_right
-- и посмотрите, удалось ли избежать описанных там ошибок
newtype ListT m a = ListT { runListT :: m [a] }

instance (Functor m) => Functor (ListT m) where
  -- fmap f = mapListT $ fmap $ map f
  fmap f m = ListT $ (fmap $ map f) (runListT m)
instance (Applicative m) => Applicative (ListT m) where
  pure a  = ListT $ pure [a]
  f <*> v = ListT $ (<*>) <$> runListT f <*> runListT v
instance (Monad m) => Monad (ListT m) where
  return a = ListT $ return [a]
  m >>= k  = ListT $ do
    a <- runListT m
    b <- mapM (runListT . k) a
    return (concat b)
instance MonadTrans ListT where
  lift m = ListT $ do
    a <- m
    return [a]
