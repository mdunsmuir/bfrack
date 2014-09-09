module Grid (Grid, replicate, read, write) where

import Prelude hiding (replicate, read)
import qualified Prelude as P
import Control.Monad
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM

type Grid a = V.Vector (VM.IOVector a)

replicate :: Int -> Int -> a -> IO (Grid a)
replicate w h a = (liftM V.fromList) $ forM (P.replicate h a) $ VM.replicate w

wrapCoordsDo f g x y = let h = V.length g
                           w = VM.length $ V.head g
                           x' = x `mod` w
                           y' = y `mod` h
                       in  f g x' y'

read :: Grid a -> Int -> Int -> IO a
read = wrapCoordsDo read'
read' grid x y = VM.read (grid V.! y) x

write :: Grid a -> Int -> Int -> a -> IO ()
write = wrapCoordsDo write'
write' grid x y = VM.write (grid V.! y) x
