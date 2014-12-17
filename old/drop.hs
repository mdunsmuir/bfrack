myDrop :: Int -> [a] -> [a]
myDrop n list = 
  if n <= 0 || null list
  then list
  else myDrop (n - 1) (tail list)
