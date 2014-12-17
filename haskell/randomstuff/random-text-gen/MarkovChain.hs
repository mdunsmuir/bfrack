module MarkovChain where

import Data.List
import Control.Monad.ST
import qualified Data.HashTable.ST.Basic as Hash
import System.Random

data MarkovNode a = MarkovNode a [(Float, a)] | End deriving Show
type MarkovChain a = Hash.HashTable a (MarkovNode a)

nextNode gen End _ = return (gen, Nothing)
nextNode gen (MarkovNode _ nexts) chain =
  let 
    sumProb = foldl1' (+) $ map (\(p, _) -> p) nexts
    (r, newGen) = randomR (0, sumProb) gen :: (Float, StdGen)

    folder cumList@((cumP, _):_) (p, v) =
      ((p + cumP), v):cumList

    (_, dummyVal) = head nexts -- dummy value for memo array
    cumProbs = tail $ reverse $ foldl folder [(0.0, dummyVal)] nexts

    (_, next) = head $ dropWhile (\(p, _) -> r > p) cumProbs
  in 
    if null nexts
    then return (newGen, Nothing)
    else do next <- Hash.lookup chain next
            return (newGen, next)

createChainFromNodes nodes = 
   do chain <- Hash.new
      let f node@(MarkovNode name _) = Hash.insert chain name node
      mapM_ f nodes 
      return chain

getSequenceForKey gen key chain = 
  do start <- Hash.lookup chain key
     case start of
       (Just node) -> getSequence gen node chain
       Nothing     -> return []

getSequence gen start@(MarkovNode payload _) chain = 
  do (newGen, next) <- nextNode gen start chain
     case next of
       (Just n) -> do rest <- getSequence newGen n chain
                      return $ payload : rest
       Nothing  -> return [payload]

testChain = createChainFromNodes nodes
  where nodes = [MarkovNode 'a' [(0.4, 'e'), (0.6, 'a'), (0.1, 'q')],
                 MarkovNode 'e' [(0.3, 'e'), (0.7, 'a')]]

runTestChain =
  do gen <- getStdGen
     return $ runST $ testChain >>= getSequenceForKey gen 'a'
