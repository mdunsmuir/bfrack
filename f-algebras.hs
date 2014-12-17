-- common

data Fix a = Fx (a (Fix a))

unfix :: Fix a -> a (Fix a)
unfix (Fx f) = f

type Algebra f a = f a -> a

cata :: Functor f => (f a -> a) -> Fix f -> a
cata alg = alg . fmap (cata alg) . unfix

-- expression
 
data ExprF a = Const Int
             | Add a a
             | Mul a a

instance Functor ExprF where
  fmap eval (Const i) = Const i
  fmap eval (left `Add` right) = (eval left) `Add` (eval right)
  fmap eval (left `Mul` right) = (eval left) `Mul` (eval right)

type Expr = Fix ExprF
type SimpleA = Algebra ExprF Int

alg :: SimpleA
alg (Const i)   = i
alg (x `Add` y) = x + y
alg (x `Mul` y) = x * y

eval :: Expr -> Int
eval = alg . (fmap eval) . unfix

-- list

data ListF a b = Nil | Cons a b

instance Functor (ListF a) where
  fmap f Nil = Nil
  fmap f (Cons e x) = Cons e (f x)

type List a = Fix (ListF a)

fromList :: [a] -> Fix (ListF a)
fromList [] = Fx Nil
fromList (x:xs) = Fx $ Cons x $ fromList xs

algSum :: Algebra (ListF Int) Int
algSum Nil = 0
algSum (Cons x acc) = x + acc

algFold :: (a -> b -> b) -> b -> Algebra (ListF a) b
algFold f init Nil = init
algFold f _ (Cons x acc) = f x acc

foldr' :: (a -> b -> b) -> b -> List a -> b
foldr' f init = cata $ algFold f init
