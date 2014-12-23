import MaybeT

getOdd :: IO (Maybe Int)
getOdd = do
  str <- getLine
  let x = read str :: Int
  if odd x then return (Just x) else return Nothing

divisibleBy5 :: Int -> Maybe Int
divisibleBy5 x = if x `mod` 5 == 0 then Just x else Nothing

maybeSequence :: MaybeT IO String
maybeSequence = do
  str <- lift getLine
  return $ str
