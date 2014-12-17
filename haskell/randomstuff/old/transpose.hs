import System.Environment (getArgs)

interactWith function inputFile outputFile = do
  input <- readFile inputFile
  writeFile outputFile (function input)

main = mainWith myFunction
  where mainWith function = do
          args <- getArgs
          case args of
            [input,output] -> interactWith function input output
            _ -> putStrLn "error: exactly two arguments needed"

        -- replace "id" with the name of our function below
        myFunction = transposeContent
     
transposeContent content = transposedLines
  where
    myLines = lines content
    
transpose :: [[a]] -> [[a]]
transpose [(x:xs)]
transpose (row:rows) = zipWith (:) row $ transpose rows
