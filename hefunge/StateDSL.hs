module StateDSL (
  Befunge,
  BefungeState,
  Direction (..),
  move, advance,
  pop, push,
  readGrid, readGridAt,
  writeGrid, writeGridAt,
  module Control.Monad.State.Lazy
) where

import Control.Monad.State.Lazy
import qualified Grid as G

data Direction = U | D | L | R deriving (Enum, Show)

type BefungeState = (G.Grid Char, [Int], Int, Int, Direction)
type Befunge = StateT BefungeState IO

{-
  movement
-}

move dir = do
  (grid, stk, x, y, _) <- get
  put (grid, stk, x, y, dir)

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
  stack
-}

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

{-
  grid
-}

readGrid :: Befunge Char
readGrid = do
  (_, _, x, y, _) <- get
  readGridAt x y

readGridAt :: Int -> Int -> Befunge Char
readGridAt x y = do
  (grid, _, _, _, _) <- get
  lift $ G.read grid x y

writeGrid :: Char -> Befunge ()
writeGrid c = do
  (_, _, x, y, _) <- get
  writeGridAt x y c

writeGridAt :: Int -> Int -> Char -> Befunge ()
writeGridAt x y c = do
  (grid, _, _, _, _) <- get
  lift $ G.write grid x y c
