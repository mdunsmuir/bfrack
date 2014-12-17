module KadanesAlgorithm (largestSublistSum) where

import Data.List
import Test.QuickCheck

largestSublistSum :: [Int] -> Int
largestSublistSum arr = lss' arr 0 0
  where lss' [] _ max = max
        lss' (x:xs) curSum maxSum = let curSum' = max 0 (curSum + x)
                                    in  lss' xs curSum' $ max curSum' maxSum

largestSublistSum' :: [Int] -> Int
largestSublistSum' [] = 0
largestSublistSum' xs = max 0 $ maximum sums
  where sums = map sum $ concat $ map (tail . inits) $ init $ tails xs
        
check xs = largestSublistSum xs == largestSublistSum' xs
