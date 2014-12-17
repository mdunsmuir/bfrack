import Prelude hiding (map)
import Control.Applicative hiding (empty)
import Control.Monad
import Control.Monad.State
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S

newtype Edge a = Edge (a, a) deriving (Eq, Ord, Show) -- from to
data Graph a = Graph (S.Set a) (S.Set (Edge a)) deriving Show

map :: (Ord a, Ord b) => (a -> b) -> Graph a -> Graph b
map f (Graph nodes edges) = Graph nodes' edges'
  where 
    nodesList = S.toList nodes
    mappedNodes = fmap f nodesList
    changeMap = M.fromList $ zip nodesList mappedNodes

    nodes' = S.fromList mappedNodes
    edges' = S.map (\(Edge (p, c)) -> Edge (fromJust $ M.lookup p changeMap, fromJust $ M.lookup c changeMap)) edges

empty :: Ord a => Graph a
empty = Graph S.empty S.empty

insert :: Ord a => a -> a -> Graph a -> Graph a
insert parent child graph = Graph nodes' edges'
  where (Graph nodes edges) = graph
        nodes' = S.insert parent $ S.insert child nodes
        edges' = S.insert (Edge (parent, child)) edges

treePrint :: String -> Graph String -> Int -> IO ()
treePrint root graph depth = do
  putStrLn $ replicate depth ' ' ++ root
  forM_ children $ \c -> treePrint c graph (depth + 2)
  where
    (Graph nodes edges) = graph
    children = fmap (\(Edge (p, c)) -> c) $ filter (\(Edge (p, c)) -> root == p) $ S.toList edges

levels :: Ord a => Graph a -> [[a]]
levels graph
  | S.null nodes = []
  | otherwise = (S.toList noParents) : (levels (Graph nodes' edges'))
  where 
    (Graph nodes edges) = graph
      
    -- figure out which nodes have no parents (and which do)
    childNodes = S.map (\(Edge (_, c)) -> c) edges
    (nodes', noParents) = S.partition (\n -> S.member n childNodes) nodes

    -- generate new edges list
    edges' = S.filter (\(Edge (p, _)) -> S.member p nodes') edges

fromStackOverflow :: Graph String
fromStackOverflow = foldr (\(p, c) g -> insert p c g) empty groups'
  where 
    groups' = concat $ fmap (\(p:cs) -> fmap (\c -> (p, c)) cs) groups
    groups = [["Bob","Dylan","Susan"],
              ["Dylan","Cole","Sarah"],
              ["Cole","Patrick","Patricia"],
              ["Sarah","David","Fiona"],
              ["Susan","Michael","Madeline"]]

simpleTreePrint :: [[String]] -> IO ()
simpleTreePrint list = p' (fst $ head mapping) 0
  where
    -- this recursive function prints the 'root' name (eg "Bob") that it is
    -- called with, then recursively calls itself for all the children of
    -- that name that it finds in the 'mapping' data structure
    p' :: String -> Int -> IO ()
    p' root depth = let children = maybe [] id $ lookup root mapping
                    in  do putStrLn $ replicate depth ' ' ++ root
                           forM_ children $ \c -> p' c (depth + 2)

    -- to make child lookups easier, we convert the original list of lists
    -- of names into tuples whose first values are the 'parent' name, and
    -- whose second values are the remaining names. This allows us to use the
    -- regular List lookup function, which is not efficient but may suffice
    -- for this application
    mapping :: [(String, [String])]
    mapping = fmap (\(p:cs) -> (p, cs)) list

groups = [["Bob","Dylan","Susan"],
          ["Dylan","Cole","Sarah"],
          ["Cole","Patrick","Patricia"],
          ["Sarah","David","Fiona"],
          ["Susan","Michael","Madeline"]]

----

  --data Tree a = Leaf | Tree a (Tree a) (Tree a) deriving Show
data Tree a = Leaf | Tree a (Tree a) (Tree a) deriving Show

toTree :: [[String]] -> Tree String
toTree list = toTree' root
  where
    -- both these are extremely unsafe, as they assume that the input is a list
    -- of lists each with length three
    root = fst $ head mapping
    mapping :: [(String, (String, String))]
    mapping = fmap (\(p:c1:c2:[]) -> (p, (c1, c2))) list

    -- Recursively build our tree, using the association list defined above to
    -- look up the children for each node. If there are no children, we return
    -- a Tree node with Leaf children instead.
    toTree' root = let childs = lookup root mapping
                   in  maybe (Tree root Leaf Leaf) 
                             (\(l, r) -> Tree root (toTree' l) (toTree' r)) 
                             childs

printTree :: Tree String -> IO ()
printTree t = printTree' t 0
  where 
    -- if we reached the bottom of the tree, do nothing
    printTree' Leaf _ = return ()

    -- We first print the current node's string value, and then recursively
    -- call ourselves for the children. This is a simple depth-first tree
    -- traversal, for which binary trees are well-suited.
    printTree' (Tree s l r) depth = do
      putStrLn $ replicate depth ' ' ++ s
      printTree' l (depth + 2)
      printTree' r (depth + 2)

-- tree monad
type Traverser a = StateT [Tree a] IO

goUp :: Traverser a Bool
goUp = do
  nodes@(n:ns) <- get
  if null ns 
    then return False
    else put ns >> return True

go :: Tree a -> Traverser a Bool
go node = case node of
            Leaf -> return False
            _    -> do nodes <- get 
                       put (node:nodes)
                       return True

goLeft = do
  nodes@((Tree _ l _):_) <- get
  go l
  
goRight = do
  nodes@((Tree _ _ r):_) <- get
  go r
  
current :: Traverser a a 
current = do
  ((Tree x _ _):_) <- get
  return x
  
traversePrint root = execStateT (findFirst >> tp' S.empty) [root]
  where
    tp' visited = do
      val <- current
      if not (S.member val visited)
        then do let visited' = S.insert val visited
                lift $ putStrLn val
                hasRight <- goRight
                if hasRight
                  then do findFirst
                          tp' visited'
                  else goUpOrReturn visited'
        else goUpOrReturn visited
        where goUpOrReturn vs = do
                hasUp <- goUp
                if hasUp then tp' vs else return ()
      
    findFirst = do
      went <- goLeft
      case went of
        True -> findFirst
        False -> return ()
