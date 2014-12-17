import Data.Char(toLower)

data KittyColor = Black | White | Orange deriving (Show)
data Kitty      = Kitty { name :: String, age :: Int, color :: KittyColor }

instance Show Kitty where
  show k = (name k) ++ " is a " ++ (map toLower $ show $ color k) ++ 
             " kitty who is " ++ (show $ age k) ++ " years old!"
