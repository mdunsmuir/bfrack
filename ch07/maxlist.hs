maxList (x:[])   = [x]
maxList (x:y:[]) = (max x y) : []
maxList (x:y:xs) = (max x y) : (maxList $ y:xs)

maxPathSum :: [[Int]] -> [Int]
maxPathSum (x:[]) = x
maxPathSum list = foldr lineMax (last list) (init list)

lineMax x y = zipWith (+) x (maxList y)

parseData :: String -> [[Int]]
parseData s = map intParse $ lines s
  where intParse s = map read (words s)

main = do inpstr <- readFile "triangledata.txt"
          putStrLn $ show (head (maxPathSum (parseData inpstr)))
