module RInt where

import Natural
import Data.Monoid
import Control.Monad.State
import Data.Ratio ((%))
import Test.QuickCheck hiding (Positive)

-- Integer type combines a Natural with a Sign

data Sign = Positive | Negative deriving (Eq, Show)
data RInt = RInt { num :: Natural, sign :: Sign }

neg Positive = Negative
neg Negative = Positive

instance Eq RInt where
  x == y = sign x == sign y && num x == num y

instance Ord RInt where
  x <= y
    | xs == ys = if xs == Positive 
                   then num x <= num y
                   else num x >= num y
    | otherwise = case xs of
                    Positive -> False
                    Negative -> True
    where ys = sign y
          xs = sign x

instance Monoid RInt where
  mempty = RInt Zero Positive
  mappend x y
    | sx == sy = RInt (nx `mappend` ny) sx
    | otherwise = if ny < nx
                    then RInt (nx `subtractUnsafe` ny) sx
                    else RInt (ny `subtractUnsafe` nx) sy
    where sx = sign x
          sy = sign y
          nx = num x
          ny = num y

xor Positive Negative = Negative
xor Negative Positive = Negative
xor _ _ = Positive

instance Num RInt where
  x + y = x `mappend` y

  (RInt nx sx) * (RInt ny sy)
    | nx == Zero || ny == Zero = RInt Zero Positive
    | otherwise = RInt (mul Zero nx ny) s'
    where s' = xor sx sy
          mul sum x Zero = sum
          mul sum x y' = let (Succ y'') = y'
                             sum' = sum `mappend` x
                         in  mul sum' x y''

  abs (RInt n _) = RInt n Positive
  signum (RInt Zero _) = 0
  signum (RInt _ s) = case s of
                        Positive -> 1
                        Negative -> -1

  fromInteger = intToRInt
  negate (RInt n s) = RInt n (neg s)

{-
instance Real RInt where
  toRational x = toInteger x % 1
-}

instance Enum RInt where
  toEnum = intToRInt
  fromEnum = rintToInt

{-
instance Integral RInt where
  toInteger = rintToInt
  quotRem (RInt nx sx) (RInt ny sy) = 
    (\(a, b) -> (b, a)) $ runState (qr x) (RInt Zero Positive)
    where qr x' = if y <= x'
                    then do n <- get
                            put $ n + 1
                            qr (x' - y)
                    else return x'
-}

instance Show RInt where
  show = show . rintToInt

intToRInt 0 = RInt Zero Positive
intToRInt x
  | x >= 0 = RInt (intToNatural x) Positive
  | otherwise = RInt (intToNatural $ negate x) Negative

rintToInt (RInt num sign)
  | sign == Positive = num'
  | sign == Negative = negate num'
  where num' = naturalToInt num

-- RInt tests

prop_rint_addition :: Int -> Int -> Bool
prop_rint_addition x y =
  rintToInt (intToRInt x + intToRInt y) == x + y

prop_rint_multiplication :: Integer -> Integer -> Bool
prop_rint_multiplication x y =
  rintToInt (fromInteger x * fromInteger y) == x * y
