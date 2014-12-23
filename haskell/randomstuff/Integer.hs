module RecursiveInteger where

import Data.Monoid

-- Natural type

data Natural = Zero
             | Succ Natural
             deriving Show

instance Monoid Natural where
  mempty = Zero
  mappend Zero y = y
  mappend (Succ n) y = Succ $ mappend n y

-- Integer type combines a Natural with a Sign

data Sign = Positive | Negative deriving Show
data RecursiveInteger = RecursiveInteger Natural Sign deriving Show

mkRecursiveInteger :: Integer -> RecursiveInteger
mkRecursiveInteger x
  | x >= 0 = RecursiveInteger (mkNatural' x) Positive
  otherwise = RecursiveInteger (mkNatural' (negate x)) Negative
  where 
    mkNatural' 0 = Zero
    mkNatural' x = Succ $ mkNatural' (x - 1)
