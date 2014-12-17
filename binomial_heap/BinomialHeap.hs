{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}

module BinomialHeap (
  fromList, toList,
  insert, findMin, deleteMin, merge,
  checkAll, checkOrders, checkFindMin, checkDeleteMin
) where

import Data.List hiding (insert)
import qualified Data.Map.Lazy as M

class Heap h a | h -> a where
  insert :: a -> h -> h
  findMin :: h -> Maybe a
  deleteMin :: h -> h
  empty :: h -> Bool
  merge :: h -> h -> h

--

data BinomialTree a = BinomialTree { value :: a, 
                                     subTrees :: [BinomialTree a]
                      } deriving Show

order = length . subTrees

combine :: (Eq a, Ord a) => BinomialTree a -> BinomialTree a -> BinomialTree a
combine a b = if value a < value b
                then BinomialTree (value a) (b : subTrees a)
                else BinomialTree (value b) (a : subTrees b)

instance (Eq a) => Eq (BinomialTree a) where
  a == b = (order a) == (order b)

instance (Eq a, Ord a) => Ord (BinomialTree a) where
  a <= b = (order a) <= (order b) 

---

newtype BinomialHeap a = BinomialHeap [BinomialTree a] deriving Show

fromList :: (Eq a, Ord a) => [a] -> BinomialHeap a
fromList = foldr insert (BinomialHeap [])

toList :: (Eq a, Ord a) => BinomialHeap a -> [a]
toList h = unfoldr unf h
  where unf h' = case findMin h' of
                   Just min' -> Just (min', deleteMin h')
                   Nothing -> Nothing

instance (Eq a, Ord a) => Heap (BinomialHeap a) a where
  insert x h = merge h (BinomialHeap [BinomialTree x []])

  findMin h'@(BinomialHeap h)
    | empty h' = Nothing
    | otherwise = Just $ minimum $ map value h

  deleteMin h'@(BinomialHeap h)
    | empty h' = h'
    | otherwise = 
        merge (BinomialHeap (subTrees minTree)) (BinomialHeap withoutMin)
        where withoutMin = deleteBy (\m t -> value m == value t) minTree h 
              minTree = minimumBy (\a b -> (value a) `compare` (value b)) h

  empty (BinomialHeap []) = True
  empty _ = False

  merge (BinomialHeap a) (BinomialHeap b) = BinomialHeap $ map snd $ M.toList mb
    where ma = foldr (\t m -> M.insert (order t) t m) M.empty a
          mb = foldr (\t m -> addToMap t m) ma b
          addToMap t m = case M.lookup (order t) m of
                           Just t' -> let m' = M.delete (order t) m
                                      in  addToMap (combine t t') m'
                           Nothing -> M.insert (order t) t m
-- tests

checkAll xs = checkOrders xs && checkFindMin xs && checkDeleteMin xs

checkMerge :: [Int] -> [Int] -> Bool
checkMerge xs ys = (sort (xs ++ ys)) == (toList $ merge (fromList xs) (fromList ys))

checkMergeOrders :: [Int] -> [Int] -> Bool
checkMergeOrders xs ys = let (BinomialHeap ts) = merge (fromList xs) (fromList ys)
                         in length (nub ts) == length ts

checkOrders :: [Int] -> Bool
checkOrders xs
  | null xs = True
  | otherwise = let (BinomialHeap ts) = fromList xs
                in  length (nub ts) == length ts

checkFindMin :: [Int] -> Bool
checkFindMin xs@[] = case findMin (fromList xs) of
                       Nothing -> True
                       _       -> False
checkFindMin xs = case findMin (fromList xs) of
                    Just x -> minimum xs == x
                    _      -> False

checkDeleteMin xs = sort xs == (toList $ fromList xs)
