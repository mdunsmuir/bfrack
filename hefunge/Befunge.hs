{-# LANGUAGE DeriveDataTypeable #-}

module Befunge (
  newState, 
  befunge,
  Befunge,
  BefungeState
) where

import Prelude hiding (subtract, not)
import System.IO
import Control.Exception
import Data.Typeable
import Data.Char
import System.Random hiding (random)
import qualified Data.Map.Strict as M

import StateDSL
import qualified Grid as G (Grid)

data BadInstructionException = BadInstructionException Char deriving (Show, Typeable)
instance Exception BadInstructionException

befunge = do
  char <- readGrid
  case instructionFor char of
    (Just func) -> func >> advance >> befunge
    Nothing     -> case char of
                     '@' -> return ()
                     _   -> throw $ BadInstructionException char

newState :: G.Grid Char -> BefungeState
newState grid = (grid, [], 0, 0, R)

instructionFor :: Char -> Maybe (Befunge ())
instructionFor = flip M.lookup instructionMap

instructionMap :: M.Map Char (Befunge ())
instructionMap = M.fromList [
  ('0', push 0), ('1', push 1), ('2', push 2), ('3', push 3), ('4', push 4),
  ('5', push 5), ('6', push 6), ('7', push 7), ('8', push 8), ('9', push 9),
  ('+', add), ('-', subtract), ('*', multiply), ('/', divide), ('%', modulo),
  ('!', not), ('`', greater),
  ('^', move U), ('>', move R), ('v', move D), ('<', move L),
  ('?', random), ('#', advance),
  ('_', cond R L), ('|', cond D U), ('$', pop >> return ()), (':', duplicate), ('\\', swap),
  ('"', string),
  ('p', gridPut), ('g', gridGet),
  ('&', intInput), ('~', charInput), ('.', intOutput), (',', charOutput),
  (' ', return ()) ]

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

random = do
  r <- lift $ getStdRandom (randomR (0, 3))
  let dir = toEnum r :: Direction
  move dir

{-
  control
-}

cond d1 d2 = do
  a <- pop
  if a == 0 then move d1 else move d2

duplicate = do
  a <- pop
  push a
  push a

swap = do
  a <- pop
  b <- pop
  push a
  push b

{-
  string entry
-}

string = advance >> string'
string' = do
  char <- readGrid
  case char of
    '"' -> return ()
    _   -> push (fromEnum char) >> advance >> string'

{-
  storage
-}

gridPut = do
  y <- pop
  x <- pop
  v <- pop
  let c = toEnum v :: Char
  writeGridAt x y c

gridGet = do
  y <- pop
  x <- pop
  v <- readGridAt x y
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
