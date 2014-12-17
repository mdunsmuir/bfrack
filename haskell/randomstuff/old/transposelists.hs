import Data.List

myTranspose :: [[a]] -> [[a]]
myTranspose list = 
  case filteredList of 
    [] -> []
    _ -> (map head filteredList) : (myTranspose $ map tail filteredList)
  where filteredList = filter (not . null) list

testEqualFuncs func1 func2 input =
  (func1 input) == (func2 input)
