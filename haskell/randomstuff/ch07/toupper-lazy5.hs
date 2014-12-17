-- file: ch07/toupper-lazy5.hs
import Data.Char(toUpper)

main = interact $ (++) "Your data, in uppercase, is:\n\n" . map toUpper
