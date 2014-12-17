main = do interact eachLine
  where eachLine = unlines . map rpn . lines

rpn = show . head . foldl rpn' [] . words
  where rpn' stk c = let oper (x:y:stk) f = f x y : stk
                     in  case c of
                           "+" -> oper stk (+)
                           "-" -> oper stk (-)
                           "*" -> oper stk (*)
                           "/" -> oper stk (/)
                           x   -> (read x :: Double) : stk
