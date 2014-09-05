module Detector (detector,dumpToFile,dumpToConsole,pipeOver,
                         xcoord,binner,histPipe,pushEvery) where

import Pipes
import Pipes.Lift
import qualified Pipes.Prelude as P
import Neutron
import Vec

import Control.Monad(forever,replicateM_)
import Control.Monad.Trans.State.Strict

import System.IO

detector :: (Num a, Show a) => Consumer (Neutron a) IO ()
detector = forever $ do
  temp <- await
  lift $ print temp

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

binner' :: Monad m => Int -> Pipe a [a] (StateT [a] m) b
binner' bins = forever $
         do
           next <- await
           current <- lift get
           if length current >= bins
           then do
             lift $ put []
             yield . reverse $ next : current
           else
               lift $ put $ next : current

binner :: Monad m => Int -> Pipe t [t] m r
binner bins = evalStateP [] $ binner' bins

pipeOver' :: Monad m => (a -> b -> b) -> Pipe a b (StateT b m )r
pipeOver' f =  forever $
         do
           next <- await
           current <- lift get
           let result = f next current
           lift $ put result
           yield result


pipeOver :: Monad m => b -> (a -> b -> b) -> 
            Pipe a b m r
pipeOver initial = evalStateP initial . pipeOver'

xcoord :: Pipe (Neutron a) a IO ()
xcoord = forever $ do
           temp <- await
           yield $ x . position $ temp



--- Pipe for performing histogramming



toBin :: Int -> (Double,Double) -> Double -> Int
toBin steps (low,high) n = let temp = fromIntegral steps * (n-low)/(high-low)
                         in
                           if temp < 0
                           then 0
                           else if round temp > steps-1
                                then steps-1
                                else round temp

updateList :: Int -> a -> [a] -> [a]
updateList index n xs = take (index-1) xs ++ (n : drop index xs)

updateBins' :: Int -> [Int] -> [Int]
updateBins' n xs = updateList (n+1) (1 + xs !! n) xs

updateBins :: (Double->Int) -> Double -> [Int] -> [Int]
updateBins f n = updateBins' (f n)

histPipe :: Monad m => (a -> Double) -> Int -> (Double,Double) -> Pipe a [Int] m r
histPipe f bins range = pipeOver zeroList updater
    where
      zeroList = replicate bins 0
      updater = updateBins (toBin bins range) . f

pushEvery :: (Monad m) => Int -> Pipe a a m r
pushEvery n = forever $ do
                replicateM_ n await
                temp <- await
                yield temp

