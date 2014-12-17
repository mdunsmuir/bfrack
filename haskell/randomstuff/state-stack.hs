import Control.Monad.State

pop :: State [a] a
pop = StateT $ \(x:stk) -> (x, stk)

push x = StateT $ \stk -> ((), x : stk)
