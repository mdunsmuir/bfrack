import System.Environment
import Control.Monad.State.Lazy

import Befunge
import qualified Grid as G (fromLists)

width = 80
height = 80
debug = False

main = do
  args <- getArgs
  if length args /= 1
    then putStrLn "gotta give a filename"
    else runFile $ head args

runFile fileName = do
  program <- readFile fileName
  grid <- G.fromLists width height ' ' $ lines program
  let state = newState grid
  runStateT befunge state
  return ()
