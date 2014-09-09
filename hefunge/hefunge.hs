{-# LANGUAGE DeriveDataTypeable #-}

import Prelude hiding (subtract, not)
import System.IO
import System.Environment
import Control.Exception
import Control.Monad.State.Lazy
import Control.Concurrent
import System.Random hiding (random)
import Data.Char
import Data.Typeable
import qualified Data.Map.Strict as M
import qualified Grid as G

data BadInstructionException = BadInstructionException Char deriving (Show, Typeable)
instance Exception BadInstructionException

data Direction = U | D | L | R deriving (Enum, Show)

type BefungeState = (G.Grid Char, [Int], Int, Int, Direction)
type Befunge = StateT BefungeState IO

width = 80
height = 25
debug = False

main = do
  args <- getArgs
  if length args /= 1
    then putStrLn "gotta give a filename"
    else runFile $ head args

runFile fileName = do
  program <- readFile fileName
  grid <- G.fromLists width height ' ' $ lines program
  let initState = (grid, [], 0, 0, R)
  runStateT befunge initState
  return ()

befunge :: Befunge ()
befunge = do
  (grid, stk, x, y, _) <- get
  instruction <- lift $ G.read grid x y
  if debug then debugPrint else return ()
  case M.lookup instruction instructionMap of
    (Just func) -> func >> advance >> befunge
    Nothing     -> case instruction of
                     '@' -> return ()
                     _   -> throw $ BadInstructionException instruction

{-
  generally useful things
-}

instructionMap :: M.Map Char (Befunge ())
instructionMap = M.fromList [
  ('0', push 0), ('1', push 1), ('2', push 2), ('3', push 3), ('4', push 4),
  ('5', push 5), ('6', push 6), ('7', push 7), ('8', push 8), ('9', push 9),
  ('+', add), ('-', subtract), ('*', multiply), ('/', divide), ('%', modulo),
  ('!', not), ('`', greater),
  ('^', move U), ('>', move R), ('v', move D), ('<', move L),
  ('?', random), ('#', advance),
  ('_', cond R L), ('|', cond D U), ('$', discard), (':', duplicate), ('\\', swap),
  ('"', string),
  ('p', gridPut), ('g', gridGet),
  ('&', intInput), ('~', charInput), ('.', intOutput), (',', charOutput),
  (' ', noOperation) ]

pop :: Befunge Int
pop = do
  (grid, stk, x, y, dir) <- get
  case stk of
    []     -> return 0
    (v:vs) -> do
      put (grid, vs, x, y, dir)
      return v

push :: Int -> Befunge ()
push i = do
  (grid, stk, x, y, dir) <- get
  put (grid, (i:stk), x, y, dir)

advance :: Befunge ()
advance = do
  (grid, stk, x, y, dir) <- get
  let (x', y') = case dir of
                   U -> (x, y - 1)
                   D -> (x, y + 1)
                   L -> (x - 1, y)
                   R -> (x + 1, y)
  put (grid, stk, x', y', dir)

{-
  arithmetic opers
-}

arithOper oper = do
  a <- pop
  b <- pop
  push (a `oper` b)

add = arithOper (+)
subtract = arithOper $ flip (-)
multiply = arithOper (*)
divide = arithOper $ flip div
modulo = arithOper $ flip mod

{-
  boolean opers
-}

not = do
  a <- pop
  if a == 0 then push 1 else push 0

greater = do
  a <- pop
  b <- pop
  if b > a then push 1 else push 0

{-
  movement
-}

move dir = do
  (grid, stk, x, y, _) <- get
  put (grid, stk, x, y, dir)

random = do
  (grid, stk, x, y, _) <- get
  r <- lift $ getStdRandom (randomR (0, 3))
  let dir = toEnum r :: Direction
  put (grid, stk, x, y, dir)

{-
  control
-}

cond d1 d2 = do
  a <- pop
  if a == 0 then move d1 else move d2

discard = pop >> return ()

duplicate = do
  a <- pop
  push a
  push a

swap = do
  a <- pop
  b <- pop
  push a
  push b

noOperation = return ()

{-
  string entry
-}

string = advance >> string'
string' = do
  (grid, _, x, y, _) <- get
  char <- lift $ G.read grid x y
  case char of
    '"' -> return ()
    _   -> push (fromEnum char) >> advance >> string'

{-
  storage
-}

gridPut = do
  (grid, _, _, _, _) <- get
  y <- pop
  x <- pop
  v <- pop
  let c = toEnum v :: Char
  lift $ G.write grid x y c

gridGet = do
  (grid, _, _, _, _) <- get
  y <- pop
  x <- pop
  v <- lift $ G.read grid x y
  push $ fromEnum v
  
{-
  I/O
-}

intInput = do
  c <- lift getChar
  push $ digitToInt c

charInput = do
  c <- lift getChar
  push $ fromEnum c

intOutput = do
  lift $ hFlush $ stdout
  c <- pop
  lift $ putStr $ show c

charOutput = do
  lift $ hFlush $ stdout
  c <- pop
  lift $ putChar $ toEnum c

{-
  :(
-}

debugPrint = do
  (grid, stk, x, y, _) <- get
  instruction <- lift $ G.read grid x y
  lift $ putChar instruction
  lift $ putStrLn $ " " ++ "(" ++ (show x) ++ ", " ++ (show y) ++ ") " ++ (show stk)
  lift $ threadDelay 1000000
