import Text.Regex.Posix

--sectorNumbers :: [String] -> String -> [Int]
sectorNum thisName targetName = if (length $ head sector) == 1 then Just (read $ head sector :: Int) else Nothing
  where (_, _, _, sector) = thisName =~ ("^sector([0-9]+)" ++ targetName) :: (String, String, String, [String])
                                

{-
sectorNumbers allVariables variable = 
  where
    sectorNum thisName targetName = let (_, _, _, sector) = thisName =~ ("^sector([0-9]+)" ++ targetName)
                                    in if length sector == 1 then Just (read sector :: Int) else Nothing
-}
    
