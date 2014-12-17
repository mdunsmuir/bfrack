data MyTree a = MyTree {
  value_of :: a,
  left_of :: MyTree a,
  right_of :: MyTree a
} | Empty deriving Show

treeAdd :: Ord a => a -> MyTree a -> MyTree a
treeAdd x Empty = MyTree x Empty Empty
treeAdd x tree = 
  case x < value_of tree of
    True -> 
      (MyTree (value_of tree) (treeAdd x (left_of tree)) (right_of tree))  
    False ->
      (MyTree (value_of tree) (left_of tree) (treeAdd x (right_of tree)))

treeFill :: Ord a => [a] -> MyTree a -> MyTree a
treeFill [] tree = tree
treeFill (x:xs) tree = treeAdd x (treeFill xs tree)

treeDepth :: MyTree a -> Int
treeDepth Empty = 0
treeDepth (MyTree _ left right) = 1 + max (treeDepth left) (treeDepth right)

