import System.Environment(getArgs)

fibo = (\f -> f ++ fiboHelper f) [0, 1]
  where fiboHelper last2 = (\s -> s : fiboHelper [last last2, s]) $ sum last2

fibo2 = 0 : 1 : (zipWith (+) fibo2 $ tail fibo2)

main = do args <- getArgs
          if length args > 0
            then do putStrLn $ show $ last $ take (read $ head args) fibo2
            else    putStrLn "required argument: number of fibonacci numbers to compute"
