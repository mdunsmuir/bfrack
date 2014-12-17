import System.Environment
import System.Random
import System.IO
import Data.List
import Control.Monad
import Text.RegexPR

data Die = Die Int Int Int deriving Show

dieRegexp = "^(\\d+)d(\\d+)(\\+(\\d+))?$"

readDie str = do (_, match) <- matchRegexPR dieRegexp str
                 num <- lookup 1 match
                 rng <- lookup 2 match
                 let mod = case lookup 4 match of
                             (Just m) -> m
                             Nothing  -> "0"
                 return $ Die (read rng) (read num) (read mod)

roll = do
  putStr "enter die: "
  dieStr <- getLine
  gen <- newStdGen
  case readDie dieStr of
    (Just (Die rng num mod)) -> let rollSum = sum $ map fst $ take (num + 1) $ 
                                              iterate (\(_, g) -> randomR (1, rng) g) (0, gen)
                                in  putStrLn $ "-> " ++ show (rollSum + mod)
    Nothing -> putStrLn $ "not a valid die: " ++ dieStr

main = hSetBuffering stdout NoBuffering >> forever roll
