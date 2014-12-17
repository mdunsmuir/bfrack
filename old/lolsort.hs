import Data.List

sortLists :: [[a]] -> [[a]]
sortLists list = sortBy lenCompare list
  where
    lenCompare a b = compare (length a) (length b)
