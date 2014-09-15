module Parser (
  ParseState (..),
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

data ParseState a = ParseState (a, String) | NoParse deriving Show
data Parser a = Parser { parse :: String -> ParseState a }

instance Monad Parser where
  return x = Parser $ \s -> ParseState (x, s)
  (Parser fs) >>= f = Parser $ \s -> case fs s of
                                       ParseState (a, s') -> let Parser f' = f a
                                                             in  f' s'
                                       NoParse -> NoParse

put :: String -> Parser ()
put x = Parser $ \_ -> ParseState ((), x)

get :: Parser String
get = Parser $ \s -> ParseState (s, s)

noParse :: Parser a
noParse = Parser $ \s -> NoParse

equals :: String -> Parser String
equals ex = do
  s <- get
  let stripped = stripPrefix ex s
  case stripped of
    Just s' -> do put s'
                  return ex
    Nothing -> noParse

charsSatisfying :: (Char -> Bool) -> Parser String
charsSatisfying f = do
  s <- get
  let (match, remain) = span f s
  if null match
    then noParse
    else put remain >> return match

maybeParse :: Parser a -> Parser (Maybe a)
maybeParse parser = do
  initial <- get
  case parse parser initial of
    ParseState (result, remain) -> do put remain
                                      return $ Just result
    NoParse -> return Nothing

letters = charsSatisfying isLetter
numbers = charsSatisfying isDigit
spaces = charsSatisfying isSpace

chompSpaces = do
  str <- get
  let (_, remain) = span isSpace str
  put remain

try p = liftM Just p <|> return Nothing

infixl 7 <|>

(<|>) :: Parser a -> Parser a -> Parser a
p1 <|> p2 = do
  initial <- get
  let a1 = parse p1 initial
      a2 = parse p2 initial
  case a1 of
    ParseState (result, remain) -> put remain >> return result 
    NoParse -> case a2 of
                 ParseState (result, remain) -> put remain >> return result
                 NoParse -> noParse
