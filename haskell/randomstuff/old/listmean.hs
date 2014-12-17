listMean :: [Double] -> Maybe Double
listMean [] = Nothing
listMean list = Just ((mySum list) / (myLength list))

                where 

                  mySum :: [Double] -> Double
                  mySum []        = 0.0
                  mySum (x:xs)    = x + (mySum xs)

                  myLength :: [Double] -> Double
                  myLength []     = 0.0
                  myLength (_:xs) = 1.0 + myLength xs
                   
