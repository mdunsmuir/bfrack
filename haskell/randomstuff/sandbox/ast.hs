data AST1 a n = Output a n | Bell n | Done

data Fix f = Fix (f (Fix f))
