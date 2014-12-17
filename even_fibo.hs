evenFiboSum max = sum $ filter even $ takeWhile (<= max) fibos
  where fibos = 0 : 1 : (zipWith (+) fibos $ tail fibos)
