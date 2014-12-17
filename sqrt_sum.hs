numSquares max = length $ takeWhile (<= max) $ scanl (+) 0 $ map sqrt [1..]
