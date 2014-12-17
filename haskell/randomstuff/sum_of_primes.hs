import Primes

main = do putStrLn $ show $ sum $ takeWhile (<2000000) primes
