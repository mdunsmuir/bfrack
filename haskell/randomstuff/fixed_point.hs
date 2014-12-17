data ExprF a = Const Int
             | Add a a
             | Subtract a a

data Fix f = Fx (f (Fix f))

type Expr = Fix ExprF

instance Functor ExprF where
  fmap f (Const i) = Const i
  fmap f (a `Add` b) = f a `Add` f b
  fmap f (a `Subtract` b) = f a `Subtract` f b

-- state monad

data State s a = State { runState :: s -> (a, s) }

instance Monad (State s) where
  return x = State $ \s -> (x, s)
  x >>= f = State $ \s -> let (a, s') = runState x s
                              f' = f a
                          in  runState f' s'

get :: State s s
get = State $ \s -> (s, s)

put :: s -> State s ()
put x = State $ \_ -> ((), x)

f = do
  s <- get
  put (s + 1)
  t <- get
  return (t + 3)

f' = get >>= (\s -> put (s + 1))
