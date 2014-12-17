lastButOne :: [a] -> a
lastButOne list =
  if length list /= 2
  then lastButOne (tail list)
  else head list
