import Control.Monad
import Data.Conduit
import qualified Data.Conduit.List as CL

mySourceList :: Monad m => [a] -> Source m a
mySourceList l = forM_ l yield

source :: Source IO Int -- produces a stream of Ints
source = mySourceList [1..4]

sink :: Sink String IO () -- consumes a stream of Strings, no result
sink = CL.mapM_ putStrLn

conduit :: Conduit Int IO String -- converts Ints into Strings
conduit = CL.map show

add2 :: Conduit Int IO Int -- converts Ints into Strings
add2 = CL.map (+2)

main = do
    source $$ conduit =$ sink
    -- alternatively, with the same meaning
    source $= conduit $$ sink
    source $= add2 $= conduit $$ sink
