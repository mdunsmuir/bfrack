import Data.List

allRtTri :: Integral a => a -> [(a, [(a, a, a)])]
allRtTri upTo = filter (not . null . snd) $ zip [1..] $ map rtTri [1..upTo]

rtTri :: Integral a => a -> [(a, a, a)]
rtTri p = [(a, b, c) | c <- [1..50], a <- [1..c], b <- [1..a], 
                       a + b + c == p, a^2 + b^2 == c^2]
