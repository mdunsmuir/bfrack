import Data.List

split :: Eq a => [a] -> a -> [[a]]
split xs s = filter (not . null) $ split' xs s
  where split' xs s
          | null rest = [word]
          | otherwise = word : split' (tail rest) s
          where (word, rest) = span (/= s) xs

split' :: Eq a => [a] -> a -> [[a]]
split' xs s = let f y (x:xs)
                    | y == s = [] : x : xs
                    | otherwise = (y : x) : xs
              in  filter (not . null) $ foldr f [[]] xs

inits' [] = []
inits' (x:xs) = [x] : map (x :) (inits' xs)
