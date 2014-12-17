import Data.List
 
cMaxLength = maximum . (flip zip) [1..] . cLengths

cLengths max = take max $ map f [1..]
  where f = (+1) . length . takeWhile (/=1) . iterate next
        next n | even n    = n `quot` 2
               | otherwise = (3 * n + 1)

main = do putStrLn $ show $ cMaxLength 1000000
