import Data.Conduit
import Control.Monad.IO.Class

source :: Source IO Int
source = do
    yield 1
    yield 2
    yield 3
    yield 4
    
conduit :: Conduit Int IO String
conduit = do
    -- Get all of the adjacent pairs from the stream
    mi1 <- await
    mi2 <- await
    case (mi1, mi2) of
        (Just i1, Just i2) -> do
            yield $ show (i1, i2)
            leftover i2
            conduit
        _ -> return ()

myAwaitForever :: Monad m => (a -> Conduit a m b) -> Conduit a m b
myAwaitForever f = do
  val <- await
  case val of
    Just val' -> f val' >> myAwaitForever f
    Nothing -> return ()

sink :: Sink String IO ()
sink = myAwaitForever $ \s -> liftIO $ putStrLn s
            
main = source $$ conduit =$ sink
