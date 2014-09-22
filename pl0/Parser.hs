module Parser (
  ParseResult (..),
  Parser (..),
  noParse,
  maybeParse,
  equals, charsSatisfying,
  letters, numbers, spaces, chompSpaces,
  try, (<|>)
) where

import Data.List
import Data.Char
import Control.Monad

-- this is basically a state monad with the twist that you can abort mid-parse

data ParseResult a = ParseResult a String | NoParse String deriving Show
data Parser a = Parser { parse :: String -> ParseResult a }

instance Monad Parser where
  return x = Parser $ \s -> ParseResult x s
  (Parser fs) >>= f = Parser $ \s -> case fs s of
                                       ParseResult a s' -> let Parser f' = f a
                                                          in  f' s'
                                       NoParse msg -> NoParse msg

{-
  for internal use only
-}
put :: String -> Parser ()
put x = Parser $ \_ -> ParseResult () x

get :: Parser String
get = Parser $ \s -> ParseResult s s

{-
  this function injects a NoParse into the monad, halting all parsing
-}
noParse :: String -> Parser a
noParse msg = Parser $ \s -> NoParse msg

{-
  parse the given string
  if parsing fails, call noParse
  returns the string
-}
equals :: String -> Parser String
equals ex = do
  s <- get
  let stripped = stripPrefix ex s
  case stripped of
    Just s' -> do put s'
                  return ex
    Nothing -> noParse $ "expected \"" ++ ex ++ "\""

{-
  parse characters satisfying the given predicate
  if none are found, calls noParse
  returns the parsed characters
-}
charsSatisfying :: (Char -> Bool) -> Parser String
charsSatisfying f = do
  s <- get
  let (match, remain) = span f s
  if null match
    then noParse $ "did not find expected character(s)"
    else put remain >> return match

{-
  attempt to run the given parser
  if it fails, reset unparsed string to pre-parse state and return Nothing
  otherwise return the result of the parsed
-}
maybeParse :: Parser a -> Parser (Maybe a)
maybeParse parser = do
  initial <- get
  case parse parser initial of
    ParseResult result remain -> do put remain
                                    return $ Just result
    NoParse _ -> return Nothing

{-
  obvious: parse one or more letters, numbers, or spaces
-}
letters = charsSatisfying isLetter
numbers = charsSatisfying isDigit
spaces = charsSatisfying isSpace

{-
  parse zero or more whitespace characters
-}
chompSpaces :: Parser ()
chompSpaces = do
  str <- get
  let (_, remain) = span isSpace str
  put remain

{-
  try the given parser
  if it succeeds, return Just the result
  otherwise Nothing
  note that this function does not reset the unparsed string
  so it probably shouldn't be used
-}
try p = liftM Just p <|> return Nothing

{-
  'or' operation on parsers
  try the first parser, then the second if the first fails.
  meant to be chained together
-}
infixl 7 <|>
(<|>) :: Parser a -> Parser a -> Parser a
p1 <|> p2 = do
  initial <- get
  let a1 = parse p1 initial
      a2 = parse p2 initial
  case a1 of
    ParseResult result remain -> put remain >> return result 
    NoParse _ -> case a2 of
                   ParseResult result remain -> put remain >> return result
                   NoParse msg -> noParse msg
