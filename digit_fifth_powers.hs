answer = scanl1 (+) $ eqToSum 5
eqToSum pow = filter (\x -> x == digitPowSum pow x) [2..]
digitPowSum pow num = sum $ map (^pow) $ map (read . (:[])) $ show num
