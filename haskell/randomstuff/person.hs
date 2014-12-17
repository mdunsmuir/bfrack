data Person = Person { name :: String, age :: Int } deriving (Show)

data Person2 = Person String Int deriving (Show)

age1Year Person { name = n, 
                  age = a } = Person { name = n, age = (a + 1) }
