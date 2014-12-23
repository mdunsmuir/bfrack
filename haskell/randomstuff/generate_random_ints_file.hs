import Control.Monad.CryptoRandom
import Crypto.Random
main = do
  g <- newGenIO :: IO SystemRandom
  let rs = Prelude.take (2^20) (map abs (crandoms g) :: [Int])
  writeFile "rands" (unlines $ map show rs)
