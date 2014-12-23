module QuickSort where

import Data.List
import Control.Monad.ST
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Algorithms.Intro as I
import Test.QuickCheck

dumbQuicksort :: (Ord a) => [a] -> [a]
dumbQuicksort []   = []
dumbQuicksort list = let (l, r) = partition (< head list) $ tail list
                     in  (dumbQuicksort l) ++ [head list] ++ (dumbQuicksort r)

quickSort :: Ord a => V.Vector a -> V.Vector a
quickSort v = runST $ do
  mv <- V.thaw v
  quickSortM mv
  v' <- V.freeze mv
  return v'

quickSortM :: Ord b => MV.STVector a b -> ST a ()
quickSortM xs
  | len <= 1 = return ()
  | otherwise = do
      iPivot <- qPartition 0 1
      if iPivot > 1 
        then quickSortM $ MV.unsafeSlice 0 iPivot xs else return ()
      if iPivot < len - 2 
        then quickSortM $ MV.unsafeSlice (iPivot + 1) (len - iPivot - 1) xs else return ()
  where 
    len = MV.length xs
    qPartition iPivot i
      | i >= len = return iPivot
      | otherwise = do 
          vPivot <- MV.unsafeRead xs iPivot
          vi <- MV.unsafeRead xs i
          if vi < vPivot
            then do 
              MV.unsafeSwap xs i (iPivot + 1)
              MV.unsafeSwap xs iPivot (iPivot + 1)
              qPartition (iPivot + 1) (i + 1)
            else
              qPartition iPivot (i + 1)

quickSort' :: Ord a => V.Vector a -> V.Vector a
quickSort' vxs = runST $ do
  mvxs <- V.thaw vxs
  I.sort mvxs
  vxs' <- V.freeze mvxs
  return vxs'

prop_sort :: [Int] -> Bool
prop_sort xs = (sort xs) == V.toList (quickSort (V.fromList xs))
