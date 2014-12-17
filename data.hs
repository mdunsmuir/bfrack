data Bottle a = FullOf a | Empty deriving (Show)

instance Functor Bottle where
  fmap f (FullOf x) = FullOf $ f x
  fmap _ Empty      = Empty
