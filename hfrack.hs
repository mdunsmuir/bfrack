{-# LANGUAGE DeriveDataTypeable #-}

import qualified Data.ByteString as B
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import Control.Exception
import Data.Typeable
import Control.Monad.ST
import Data.Word

data MismatchedBracesException = MismatchedBracesException deriving (Show, Typeable)
instance Exception MismatchedBracesException

data MapIndex = Index Int | None deriving Show

initSize = 10000

brainfuck :: V.Vector Word8 -> IO ()
brainfuck prog = do
  tape <- MV.new initSize
  progMap <- mapProg prog
  runBF tape prog 0 0 progMap

runBF tape prog iTape iProg progMap
  | iProg == V.length prog = return () 
  | otherwise = case prog V.! iProg of

                  60 -> runBF tape prog (iTape - 1) iProg progMap -- <
                  62 -> runBF tape prog (iTape + 1) iProg progMap -- > 

                  43 -> tapeOper (+1) -- +
                  45 -> let sub y x = x - y
                        in  tapeOper (sub 1) -- -

                  46 -> do byte <- MV.read tape iTape -- .
                           putChar $ wordToChar byte
                           runBF tape prog iTape iProg progMap

                  91 -> do byte <- MV.read tape iTape
                           case byte of
                             1 -> let newIProg = case progMap V.! iProg of Index i -> i
                                  in  runBF tape prog iTape newIProg progMap
                             _ -> runBF tape prog iTape (iProg + 1) progMap

                  93 -> do byte <- MV.read tape iTape
                           case byte of
                             1 -> let newIProg = case progMap V.! iProg of Index i -> i
                                  in  runBF tape prog iTape newIProg progMap
                             _ -> runBF tape prog iTape (iProg + 1) progMap
                  
  where tapeOper oper = do curVal <- MV.read tape iTape
                           MV.write tape iTape (oper curVal)
                           runBF tape prog iTape iProg progMap

-- map stuff

charToWord :: Char -> Word8
charToWord = toEnum . fromEnum
wordToChar :: Word8 -> Char
wordToChar = toEnum . fromEnum

strToBStr = B.pack . map charToWord
bStrToStr = map wordToChar . B.unpack

getProgMap :: String -> IO [MapIndex]
getProgMap prog = do
  let vProg = V.fromList $ map charToWord prog
  indicesV <- mapProg vProg
  return $ V.toList indicesV

mapProg :: V.Vector Word8 -> IO (V.Vector MapIndex)
mapProg prog
  | nLBrackets /= nRBrackets = throwIO MismatchedBracesException
  | otherwise = do
      map' <- MV.replicate (V.length prog) None :: IO (MV.IOVector MapIndex)
      mapProgBackwards map' prog 0 []
      mapProgForwards map' 0
      V.freeze map'

  where
    nLBrackets = V.length $ V.filter (91 ==) prog
    nRBrackets = V.length $ V.filter (93 ==) prog

    mapProgBackwards :: MV.IOVector MapIndex -> V.Vector Word8 -> Int -> [MapIndex] -> IO ()
    mapProgBackwards map' prog iProg bStack
      | iProg == V.length prog = return ()
      | progVal == 93 && null bStack = throwIO MismatchedBracesException

      | otherwise = case progVal of
                      91 -> mapProgBackwards map' prog (iProg + 1) (Index iProg : bStack)
                      93 -> let (bi:bis) = bStack
                            in  MV.write map' iProg bi >> 
                                  mapProgBackwards map' prog (iProg + 1) bis
                      _ -> mapProgBackwards map' prog (iProg + 1) bStack

      where progVal = prog V.! iProg

    mapProgForwards :: MV.IOVector MapIndex -> Int -> IO ()
    mapProgForwards map' iMap
      | iMap == MV.length map' = return ()
      | otherwise = do mapVal <- MV.read map' iMap
                       case mapVal of 
                         Index i -> MV.write map' i (Index iMap)
                         _ -> return ()
                       mapProgForwards map' (iMap + 1)
