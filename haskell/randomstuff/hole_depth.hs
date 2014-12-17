import Test.QuickCheck

deepestHole :: [Int] -> Maybe Int
deepestHole xs
  | length xs < 3   = Nothing
  | null holeDepths = Nothing
  | otherwise       = Just $ maximum holeDepths
  where
    holeDepths = filter (0<) $ map deepestHoleForIndex [1..(length xs - 2)]
    deepestHoleForIndex i = min lPeak rPeak - holeVal
      where
        holeVal = xs !! i
        lPeak= maximum $ take i xs
        rPeak = maximum $ drop (i + 1) xs

deepestHole' :: [Int] -> Maybe Int
deepestHole' xs
  | length xs < 3 = Nothing
  | maxHole < 1 = Nothing
  | otherwise = Just maxHole
  where
    lMaxes = scanl1 max $ take (length xs - 2) xs
    rMaxes = scanr1 max (drop 2 xs)
    middles = tail $ init xs
    holeDepth lMax mid rMax = min lMax rMax - mid
    maxHole = maximum $ zipWith3 holeDepth lMaxes middles rMaxes

check :: [Int] -> Bool
check xs = deepestHole xs == deepestHole' xs
