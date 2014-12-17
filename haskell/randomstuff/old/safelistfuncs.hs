safeHead :: [a] -> Maybe a
safeTail :: [a] -> Maybe [a]
safeLast :: [a] -> Maybe a
safeInit :: [a] -> Maybe [a]

safeHead [] = Nothing
safeHead (x:xs) = Just x

safeTail [] = Nothing
safeTail (x:xs) = Just xs

safeLast [] = Nothing
safeLast [x] = Just x
safeLast (x:xs) = safeLast xs

safeInit [] = Nothing
safeInit list = 
  Just (iter list)
  where
    iter [x] = []
    iter (x:xs) = x : iter xs

splitWith :: (a -> Bool) -> [a] -> [[a]]

splitWith _ [] = []
splitWith pred list =
  case first of
    [] -> resultTail
    _ -> first : resultTail
  where
    (first, rest) = break pred list
    resultTail = splitWith pred (myTail rest)
    myTail [] = []
    myTail x = tail x
