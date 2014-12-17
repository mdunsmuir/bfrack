newtype State s a = State { runState :: s -> (a, s) }

instance Monad (State s) where
  return x = State $ \s -> (x, s)
  (State fs) >>= f = State $ \s -> let (a, s') = fs s
                                       State f' = f a
                                   in  f' s' 

put :: a -> State a ()
put x = State $ \_ -> ((), x)

get :: State s s
get = State $ \s -> (s, s)

someState :: State Int Int
someState = do
  x <- get
  put (x + 1)
  y <- get
  put (y * 5)
  z <- get
  return (z `div` 10)

someState' :: State Int ()
someState' = get >>= (\x -> put (x + 1))

otherState :: State Int Int
otherState = put 5 >>= (\x -> get)
