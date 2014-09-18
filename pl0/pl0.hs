import System.Environment
import Parser
import Grammar
import Interpreter

main = do
  args <- getArgs
  if length args /= 1
    then putStrLn "gotta give a filename"
    else do code <- readFile $ head args
            case parse parseProgram code of
              ParseState (ast, _) -> run ast
              NoParse -> putStrLn "parse failed!"
