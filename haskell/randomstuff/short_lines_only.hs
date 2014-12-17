main = do interact shortLinesOnly
  where shortLinesOnly = unlines . filter ((10>=) . length) . filter (not . null) . lines
