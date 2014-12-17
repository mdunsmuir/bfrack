import Data.List
import Data.Ord
import qualified Data.MemoCombinators as Memo

collatzLength :: Integer -> Integer
collatzLength = Memo.arrayRange (1,1000000) collatzLength'
  where
    collatzLength' 1 = 1
    collatzLength' n | odd n  = 1 + collatzLength (3 * n + 1)
                     | even n = 1 + collatzLength (n `quot` 2)

main = print $ foldl1' max $ [(collatzLength n, n) | n <- [1..1000000]]
