import System.Environment
import Control.Applicative
import Data.Maybe
import Data.List
import qualified Data.Text as T
import qualified Data.Text.IO as I
import qualified Trie as T

getDictionaryConcatenation :: Ord a => [a] -> T.Trie a -> Maybe [[a]]
getDictionaryConcatenation [] t = Just []
getDictionaryConcatenation xs t
  | null pairs = Nothing
  | otherwise = (\(p, (Just ss)) -> Just (p : ss)) $ head pairs
  where
    prefixes = T.prefixesFor xs t
    suffixes = map (fromJust . flip stripPrefix xs) prefixes
    concats = map (flip getDictionaryConcatenation t) suffixes
    pairs = sortBy (\(_, Just a) (_, Just b) -> length a `compare` length b) $
              filter (isJust . snd) $ zip prefixes concats

loadDictionary :: IO (T.Trie Char)
loadDictionary = do
  words <- T.lines <$> T.toLower <$> I.readFile "/usr/share/dict/words"
  let dict = T.fromList $ map T.unpack words
  putStrLn "loaded dictionary"
  return dict

main = do
  args <- getArgs
  if length args /= 1
    then putStrLn "you must give a string"
    else do
      dict <- loadDictionary
      putStrLn $ show $ getDictionaryConcatenation (head args) dict
