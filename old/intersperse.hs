intersperse :: a -> [[a]] -> [a]

intersperse sep []     = []
intersperse sep [x]    = x
intersperse sep (x:xs) = x ++ [sep] ++ intersperse sep xs 
