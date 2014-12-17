foo :: a -> a -> IO (a, a)
foo x y = do
             return (x, y)
