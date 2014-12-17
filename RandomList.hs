import System.Random
import Control.Monad.State
import Control.Monad.Writer

getRandomList :: Random a => IO [a]
getRandomList = do
  gen <- newStdGen
  return $ execWriter $ runStateT getRandomList' gen

getRandomList' :: Random a => StateT StdGen (Writer [a]) b
getRandomList' = do
  gen <- get
  let (r, gen') = random gen
  put gen'
  tell [r]
  getRandomList'

-- doesn't work because it wraps the IO monad but tries to generate an
-- infinite list of Randoms
getRandomList2 :: Random a => Int -> IO [a]
getRandomList2 n = execWriterT (getRandomList2' n)

getRandomList2' :: Random a => Int -> WriterT [a] IO b
getRandomList2' 0 = tell [] >>= return
getRandomList2' n = do
  gen <- lift newStdGen
  let (r, _) = random gen
  tell [r]
  getRandomList2' (n - 1)
