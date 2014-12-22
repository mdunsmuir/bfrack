data Fix a = Fx (a (Fix a))

data Tree a = Leaf String
            | Tree a a

instance Functor Tree where
  fmap f (Leaf x) = Leaf x
  fmap f (Tree x y) = Tree (f x) (f y)

-- generate the 'tree' from an initial n
pairs :: Int -> Fix Tree
pairs 0 = Fx $ Leaf "42"
pairs n = Fx $ Tree (pairs (n - 1)) (pairs (n - 1))

-- the definition of the 'algebra'
stringify :: Tree String -> String
stringify (Leaf x) = x
stringify (Tree x y) = "(" ++ x ++ "," ++ y ++ ")"

-- cata
eval :: Fix Tree -> String
eval (Fx t) = stringify $ fmap eval t
