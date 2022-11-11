-- Monad 定义
-- class Applicative m => Monad m where
-- (>>=) :: m a -> (a -> m b) -> m b
-- return :: a -> m a
-- (>>) :: m a -> m b -> m b
-- m >> k = m >>= \_ -> k
-- fail :: String -> m a
-- fail s = error s

-- Identity.hs
{-# LANGUAGE DeriveFunctor #-}
newtype Identity a = Identity { runIdentity :: a } deriving (Functor)

instance Applicative Identity where
  pure a = Identity a
  (<*>) (Identity f) (Identity a) = Identity (f a)
  
instance Monad Identity where
  return a = Identity a
  Identity m >>= k = k m