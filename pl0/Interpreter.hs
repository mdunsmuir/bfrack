import Data.List
import Parser
import Grammar

parseAndEval str = case result of
                     ParseState (result', _) -> eval result'
                     NoParse -> -9999
  where result = parse parseExpression str

opers = [
  ("+", (+)),
  ("-", subtract),
  ("*", (*)),
  ("/", div) ]

eval :: PL0 -> Int
eval (Number x) = x

eval (Term left (Just (oper, right))) = oper' (eval left) (eval right)
  where (Just oper') = lookup oper opers
eval (Term left Nothing) = eval left

eval (Expression o1 t1 t2) = 
  case t2 of
    Just (o2, t2') -> let o2' = case lookup o2 opers of
                                  Just o -> o
                      in  o2' evalT1 $ eval t2'
    Nothing -> evalT1
  where evalT1 = negate' $ eval t1
        negate' = case o1 of
                   Just "-" -> negate
                   Nothing -> id
  
