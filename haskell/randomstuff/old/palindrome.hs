makePalindrome :: [a] -> [a]
makePalindrome [] = []
makePalindrome (x:xs) = (x : makePalindrome xs) ++ [x]
     
isPalindrome :: Eq a => [a]  -> Bool
isPalindrome []               = True
isPalindrome [x]              = True
isPalindrome (x:xs)
             | x == (last xs) = isPalindrome (init xs)
             | otherwise      = False
