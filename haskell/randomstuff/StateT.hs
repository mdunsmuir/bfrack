{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}

class StateMonad m s | m -> s where
  get :: m s
  put :: s -> m ()

class MonadTrans m where
  lift :: Monad n => n a -> m n a

data State s a = State { runState :: s -> (a, s) }

instance Monad (State s) where
  return x = State $ \s -> (x, s)
  m >>= f = State $ \s -> let (a, s') = runState m s
                          in  runState (f a) s'

instance StateMonad (State s) s where
  get = State $ \s -> (s, s)
  put x = State $ \_ -> ((), x)

data StateT s m a = StateT { runStateT :: s -> m (a, s) }

instance Monad m => Monad (StateT s m) where
  return x = StateT $ \s -> return (x, s)
  m >>= f = StateT $ \s -> do
              (a, s') <- runStateT m s
              runStateT (f a) s'

instance Monad m => StateMonad (StateT s m) s where
  get = StateT $ \s -> return (s, s)
  put x = StateT $ \_ -> return ((), x)

instance MonadTrans (StateT s) where
  lift f = StateT $ \s -> do
             a <- f
             return (a, s)

test2 :: StateT Int Maybe Int
test2 = do
  z <- get
  x <- lift $ Just 10
  put x
  y <- lift $ Just 2
  x' <- get
  return $ x + y + z
