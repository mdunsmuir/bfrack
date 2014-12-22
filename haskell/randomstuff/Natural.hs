module Natural where

import Data.Monoid
import Test.QuickCheck hiding (Positive)

data Natural = Zero
             | Succ Natural
             deriving Show

-- Monoid instance implements addition
instance Monoid Natural where
  mempty = Zero
  mappend Zero y = y
  mappend (Succ n) y = Succ $ mappend n y

instance Eq Natural where
  Zero == Zero = True
  (Succ x) == (Succ y) = x == y
  _ == _ = False

instance Ord Natural where
  x <= y = cmp x y
    where cmp (Succ n) (Succ m) = cmp n m
          cmp (Succ _) Zero = False
          cmp _ _ = True

subtractUnsafe x y
  | y <= x = sub x y
  | otherwise = Zero
  where sub n Zero = n
        sub (Succ n) (Succ m) = sub n m

intToNatural 0 = Zero
intToNatural x
  | x > 0 = Succ $ intToNatural (x - 1)
  | otherwise = Zero

naturalToInt Zero = 0
naturalToInt (Succ x) = 1 + naturalToInt x

-- tests

prop_eq (NonNegative x) (NonNegative y) = (x == y) == (intToNatural x == intToNatural y)
prop_ord (NonNegative x) (NonNegative y) = (x <= y) == (intToNatural x <= intToNatural y)
prop_addition (NonNegative x) (NonNegative y) = 
  (x + y) == naturalToInt (intToNatural x `mappend` intToNatural y)

