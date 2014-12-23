import QuickSort
import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Data.Text.IO as TI
import qualified Data.Attoparsec.Text as A

parseText :: A.Parser [Int]
parseText = do 
  nums <- A.many1 $ do
    num <- A.double
    A.endOfLine
    return $ round num
  A.endOfInput
  return nums

main = do
    fileData <- TI.readFile "rands"
    let parseResult = A.parseOnly parseText fileData
    case parseResult of
      Left _ -> putStrLn "parse failed!"
      Right nums -> let sorted = sort nums
                        output = T.unlines $ fmap (T.pack . show) sorted
                    in  TI.writeFile "sorted" output

sort :: [Int] -> [Int]
sort = V.toList . quickSort . V.fromList 
