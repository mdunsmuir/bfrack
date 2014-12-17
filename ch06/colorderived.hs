data Color = Red | Green | Blue
  deriving (Read, Show, Eq, Ord)

data MyType = MyType (Int -> Bool)
