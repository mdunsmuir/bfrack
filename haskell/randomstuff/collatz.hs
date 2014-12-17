import Data.List
import qualified Data.Map as Map

cLengths :: Integer -> Integer
cLengths nMax = fst $ foldl' f (0, Map.insert 1 1 Map.empty) [1..nMax]
  where 
    f (curMax, m) n = let l = len m n
                      in  (max curMax l, Map.insert n l m)

    len m n | Map.member n m = m Map.! n
            | otherwise      = 1 + (len m $ cNext n)
      where
        cNext n | even n    = n `quot` 2
                | otherwise = 3 * n + 1

main = do putStrLn $ show $ cLengths 100000
