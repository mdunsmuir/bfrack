import System.Environment
import qualified Data.ByteString as B
import Data.Char
import Data.List
import Control.Monad.ST
import qualified Data.HashTable.ST.Basic as Hash
import qualified Data.HashTable.Class as HashClass
import System.Random
import MarkovChain

main = do
  args <- getArgs
  if length args /= 2
  then putStrLn "usage: random-text-gen <number of random sentences> <input text>"
  else processFile (read $ head args) (args !! 1)

processFile :: Int -> String -> IO ()
processFile nGen fileName = do 
  fileContents <- readFile fileName
  gen <- newStdGen
  mapM_ (putStrLn . unwords) $ sequences gen nGen $ splitIntoSentences fileContents

sequences gen nGen sentences = runST sequences'
  where sequences' = 
          do chain <- addSentencesToNewMap sentences
             listPairs <- HashClass.toList chain
             let wordList = map fst listPairs
                 listLen = length wordList
                 randomFromList gen =
                   let (i, newGen) = randomR (0, listLen - 1) gen
                   in  (wordList !! i) : randomFromList newGen
                 startWords = take nGen $ randomFromList gen
             mapM (\w -> getSequenceForKey gen w chain) startWords

chainLookup word chain = 
  do node <- Hash.lookup chain word
     case node of
       (Just node') -> return node'
       Nothing     -> return $ MarkovNode word []

addSentencesToNewMap sentences =
  do chain <- Hash.new
     mapM_ (\s -> addSentenceToMap s chain) sentences
     return chain

addSentenceToMap (w1:w2:ws) chain =
  do (MarkovNode _ nexts) <- chainLookup w1 chain
     Hash.insert chain w1 (MarkovNode w1 ((1.0, w2) : nexts))
     addSentenceToMap (w2:ws) chain

addSentenceToMap (w1:[]) chain =
  do (MarkovNode _ nexts) <- chainLookup w1 chain
     Hash.insert chain w1 (MarkovNode w1 ((1.0, "an ugly hack") : nexts))
     return chain

addSentenceToMap [] chain = return chain

splitIntoSentences :: String -> [[String]]
splitIntoSentences =  filter (not . null) . foldr f [[]] . words . map toLower
  where delim s = case last s of
                  '.'       -> True
                  '!'       -> True
                  '?'       -> True
                  otherwise -> False -- should catch EOF too
        lettersOnly = filter (\c -> isLetter c || isNumber c)
        f word (last:sentences) = if delim word
                                  then [lettersOnly $ init word] : last : sentences 
                                  else (lettersOnly word : last) : sentences
