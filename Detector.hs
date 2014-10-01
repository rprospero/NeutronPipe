{-# LANGUAGE GADTs #-}
module Detector (dumpToFile,dumpToConsole,histBuilder,liftBuilder) where

import Pipes

import Control.Monad(forever)

import qualified Data.Vector.Unboxed as V
import Data.MonoTraversable

import System.IO

dumpToFile :: (Show a) => FilePath -> Consumer a IO ()
dumpToFile file = forever $ do
                    temp <- await
                    h <- lift $ openFile file AppendMode
                    lift $ hPutStr h . (++ "\n") . show $ temp
                    lift $ hClose h

dumpToConsole :: (Show a) => Consumer a IO ()
dumpToConsole = forever $ do
                  temp <- await
                  lift $ print . (++ "\n") . show $ temp

--- Pipe for performing histogramming



toBin :: Int -> (Double,Double) -> Double -> Int
toBin steps (low,high) n = let temp = fromIntegral steps * (n-low)/(high-low)
                         in
                           if temp < 0
                           then 0
                           else if round temp > steps-1
                                then steps-1
                                else round temp

updateVector :: Int -> (Double->Int) -> Double -> V.Vector Int -> V.Vector Int
updateVector bins f n v = v V.// [(idx, 1+(v V.! idx))]
    where
      idx
          | ix < 0 = 0
          | ix > bins = bins - 1
          | otherwise = ix
      ix = f n

histBuilder :: Monad m => (a -> Double) -> Int -> (Double,Double) -> Int -> Pipe a (V.Vector Int) m r
histBuilder f bins range delay = histBuilder' updater delay delay zeroList
    where
      zeroList = V.replicate bins 0
      updater = updateVector bins (toBin bins range) . f

histBuilder' :: Monad m => (a -> V.Vector Int -> V.Vector Int) -> Int -> Int -> V.Vector Int -> Pipe a (V.Vector Int) m r
histBuilder' f size 0 v = do
  event <- await
  let v2 = f event v
  yield v2
  seq v2 $ histBuilder' f size size v2
histBuilder' f size n v = do
  event <- await
  let v2 = f event v
  seq v2 $ histBuilder' f size (n-1) v2

{-# INLINABLE liftBuilder #-}
liftBuilder :: (MonoFoldable t, Monad m, Element t ~ Double) => (a -> t) -> Int -> (Double,Double) -> Int -> Pipe a (V.Vector Int) m r
liftBuilder f bins range delay = liftBuilder' updater delay delay zeroList
    where
      zeroList = V.replicate bins 0
      updater = flip (ofoldr (updateVector bins $ toBin bins range)) . f

liftBuilder' :: Monad m => (a -> V.Vector Int -> V.Vector Int) -> Int -> Int -> V.Vector Int -> Pipe a (V.Vector Int) m r
liftBuilder' f size 0 v = do
  event <- await
  let v2 = f event v
  yield v2
  seq v2 $ histBuilder' f size size v2
liftBuilder' f size n v = do
  event <- await
  let v2 = f event v
  seq v2 $ histBuilder' f size (n-1) v2
