import Data.List (foldl')
import Data.Char 

--my folds

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f z (x:xs) = myFoldl f (f z x) xs
myFoldl _ z [] = z

myFoldr f z (x:xs) = f x $ myFoldr f z xs
myFoldr _ z [] = z

-- asInt

asInt_fold :: String -> Int
asInt_fold "" = error "string cannot be empty"
asInt_fold ('-':[]) = error "string must contain at least one digit" 
asInt_fold ('-':str) = (-1) * (asInt_fold str)
asInt_fold str = foldl' step 0 str
  where step acc val = acc * 10 + digitToInt val

-- myConcat

myConcat :: [[a]] -> [a]
myConcat list = foldr step [] list
  where step acc val = acc ++ val

-- takeWhile

takeWhileRecursion :: (a -> Bool) -> [a] -> [a]
takeWhileRecursion f (x:xs) = 
  case f x of
    True -> x : takeWhileRecursion f xs
    False -> []
takeWhileRecursion _ _ = []

takeWhileFoldr f list = foldr step [] list
  where 
    step acc val | f acc = acc : val
                 | otherwise = []

-- groupBy

myGroupBy :: (a -> a -> Bool) -> [a] -> [[a]]
myGroupBy f list = foldr step [] list
  where
    step val groups | null groups = [[val]]
                    | otherwise =
                      case f val (head (head groups)) of
                        True -> (val : head groups) : (tail groups)
                        False -> [val] : groups

-- others

myAny f list = foldl' (\a v -> f v || a) False list 
