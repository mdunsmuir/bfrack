myTails :: [a] -> [[a]]
myTails xs@(_:xs') = xs : myTails xs'
myTails _ = []
