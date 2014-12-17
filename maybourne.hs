import Control.Monad

newtype Mb a = Mb { getMb :: a } deriving (Show)

instance Monad Mb where
 -- (>>=) :: Mb a => Mb a -> (a -> Mb b) -> Mb b
  (Mb x) >>= f = f x
  _ >> y = y
  return x = Mb x
