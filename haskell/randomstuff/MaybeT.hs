module MaybeT (
  MaybeT (MaybeT), runMaybeT,
  module Control.Applicative,
  module Control.Monad,
  module Control.Monad.Trans
) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance Monad m => Functor (MaybeT m) where
  fmap f fx = fx >>= return . f

instance Monad m => Applicative (MaybeT m) where
  pure = MaybeT . return . Just
  ff <*> fx = do f <- ff
                 x <- fx
                 return $ f x

instance Monad m => Monad (MaybeT m) where
  return = MaybeT . return . Just
  x >>= f = MaybeT $ do y <- runMaybeT x -- bind result of inner action to y
                        case y of
                          Just y' -> runMaybeT $ f y'
                          Nothing -> return Nothing

-- (>>=) :: Monad m => MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b

instance MonadTrans MaybeT where
  lift f = MaybeT $ do x <- f
                       return $ Just x
