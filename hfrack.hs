{-# LANGUAGE DeriveDataTypeable #-}

import System.Environment
import System.IO
import Control.Exception
import Data.List
import Data.Typeable
import Control.Monad.Writer.Lazy
import Control.Monad.State.Lazy
import Control.Monad.ST
import Data.Word
import qualified Data.ByteString as B
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

initSize = 30000

data MismatchedBracesException = MismatchedBracesException deriving (Show, Typeable)
instance Exception MismatchedBracesException

charToWord :: Char -> Word8
charToWord = toEnum . fromEnum
wordToChar :: Word8 -> Char
wordToChar = toEnum . fromEnum

main = do
  args <- getArgs
  if length args /= 1
    then putStrLn "gotta give a filename"
    else do prog <- B.readFile $ head args
            brainfuck prog
            return ()

brainfuck prog' = do
  tape <- MV.replicate initSize 0
  let state = (tape, 0, 0)
  runStateT runBF state

  where prog = V.fromList $ B.unpack prog'
        progMap = runST $ mapProg prog
        jump' = jump progMap
        runBF = do
          (_, _, iProg) <- get
          if iProg == V.length prog then return () else do
            case prog V.! iProg of
              60 -> incTape (subtract 1) >> next
              62 -> incTape (+1) >> next
              43 -> tapeOper (+1) >> next
              44 -> acceptByte >> next
              45 -> tapeOper (subtract 1) >> next
              46 -> printByte >> next
              91 -> jump' (== 0)
              93 -> jump' (/= 0)
              _  -> next
            runBF

getByte = do (tape, iTape, iProg) <- get
             lift $ MV.read tape iTape

incTape oper = do (tape, iTape, iProg) <- get
                  put (tape, oper iTape, iProg)

tapeOper oper = do (tape, iTape, iProg) <- get
                   curVal <- lift $ MV.read tape iTape
                   lift $ MV.write tape iTape (oper curVal)

acceptByte = do (tape, iTape, iProg) <- get
                lift $ hFlush stdout
                inByte <- lift getChar
                lift $ MV.write tape iTape (charToWord inByte)
            

printByte = do byte <- getByte
               lift (putChar (wordToChar byte))
               lift $ hFlush stdout

jump progMap byteCond = do (tape, iTape, iProg) <- get
                           byte <- getByte
                           if byteCond byte 
                             then put (tape, iTape, progMap V.! iProg)
                             else next

next = do (tape, iTape, iProg) <- get
          put (tape, iTape, iProg + 1)

-- functions for building the brace map

type BracePair = (Int, Int)
data Side = L | R deriving Eq
data Brace = Brace Side Int deriving Eq
instance Ord Brace where
  (Brace _ a) <= (Brace _ b) = a <= b

mapProg :: V.Vector Word8 -> ST s (V.Vector Int)
mapProg prog = do
  let bracePairs = getBracePairs $ V.toList prog
  map' <- MV.replicate (V.length prog) 0
  forM_ bracePairs $ \(l, r) -> MV.write map' l r >> MV.write map' r l
  V.freeze map'

getBracePairs :: [Word8] -> [BracePair]
getBracePairs prog
  | length lbs /= length rbs = throw MismatchedBracesException
  | null stk  = pairs
  | otherwise = throw MismatchedBracesException
  where lbs = map (Brace L) $ findIndices (91 ==) prog
        rbs = map (Brace R) $ findIndices (93  ==) prog
        bs = sort $ lbs ++ rbs

        -- what have I done????
        (stk, pairs) = runWriter $ foldM bFold [] bs
        bFold stk b@(Brace L _) = return (b : stk)
        bFold ((Brace L li):stk) b@(Brace R ri) = tell [(li, ri)] >> return stk
