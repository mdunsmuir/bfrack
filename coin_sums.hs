coinSum = tryCoin [1, 2, 5, 10, 20, 50, 100, 200, 0]

tryCoin [0] _ = 0
tryCoin coinAmts goal | newGoal == 0 = 1
                      | newGoal  > 0 = tryCoin coinAmts newGoal +
                                       tryCoin (tail coinAmts) goal
                      | otherwise    = 0
  where newGoal = goal - (head coinAmts)

main = putStrLn $ show $ coinSum 200
