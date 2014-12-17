import Data.List

quicksort :: (Ord a) => [a] -> [a]
quicksort []   = []
quicksort list = let (l, r) = partition (< head list) $ tail list
                 in  (quicksort l) ++ [head list] ++ (quicksort r)

