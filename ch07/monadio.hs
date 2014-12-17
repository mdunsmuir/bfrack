-- file: ch07/basicio.hs

{-
main = do
       putStrLn "Greetings!  What is your name?"
       inpStr <- getLine
       putStrLn $ "Welcome to Haskell, " ++ inpStr ++ "!"
-}

main = putStrLn "Greetings! Your name?" >> getLine >>= (\inpStr -> putStrLn $ "Hello, " ++ inpStr ++ "!")
