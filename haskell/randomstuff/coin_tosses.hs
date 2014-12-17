import System.Random

coinTosses :: Int -> [Bool]
coinTosses = coinTosses' . mkStdGen
  where coinTosses' gen = let (rnum, nextGen) = random gen
                          in  rnum : coinTosses' nextGen
