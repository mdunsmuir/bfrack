module Trie (
  Trie,
  empty,
  fromList,
  insert,
  prefixesFor
) where

import Data.Maybe
import Data.List (foldr)
import qualified Data.Map as M

data Trie a = Trie [[a]] (M.Map a (Trie a))
            | Leaf
              deriving Show

-- create a new, empty Trie
empty :: Ord a => Trie a
empty = Trie [] M.empty

-- create a new Trie from a list
fromList :: Ord a => [[a]] -> Trie a
fromList xs = foldr (\x t -> insert x t) empty xs

-- insert a list into a Trie
insert :: Ord a => [a] -> Trie a -> Trie a
insert xs t = insert' xs xs t

insert' [] xs' (Trie _ m) = Trie [xs'] m
insert' (x:xs) xs' (Trie s m) = Trie s m'
  where
    next = M.findWithDefault (Trie [] M.empty) x m
    m' = M.insert x (insert' xs xs' next) m

-- return all the prefixes of the given list that are present in the Trie
prefixesFor :: Ord a => [a] -> Trie a -> [[a]]
prefixesFor xs t = prefixesFor' xs t

prefixesFor' _ Leaf = []
prefixesFor' [] (Trie s _) = s
prefixesFor' (x:xs) (Trie s m) = s ++ prefixesFor' xs (M.findWithDefault Leaf x m)
