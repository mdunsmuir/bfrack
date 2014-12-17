import Data.List

search needle = any ((needle==) . take (length needle)) . tails
