data Possibly a = Yes a | No deriving Show

instance Functor Possibly where
  fmap _ No = No
  fmap f (Yes x) = Yes $ f x
