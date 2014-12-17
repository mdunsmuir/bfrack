foo x y = x + y

bar :: Num a => (a, a) -> a
bar = uncurry foo
