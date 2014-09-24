module Detector (detector,dumpToFile,dumpToConsole,pipeOver,
                         xcoord,binner,histPipe,pushEvery,histBuilder) where

import Pipes
import Pipes.Lift
import qualified Pipes.Prelude as P
import Neutron
import Linear

import Control.Monad(forever)
import Control.Monad.Trans.State.Strict

import qualified Data.Vector.Unboxed as V

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

pipeOver :: Monad m => b -> (a -> b -> b) -> 
            Pipe a b m r
--pipeOver initial = evalStateP initial . pipeOver'
pipeOver initial f = P.scan (flip f) initial id

x (V3 a _ _) = a

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

histPipe :: Monad m => (a -> Double) -> Int -> (Double,Double) -> Pipe a (V.Vector Int) m r
histPipe f bins range = pipeOver zeroList updater
    where
      zeroList = V.replicate bins 0
      updater = updateVector (toBin bins range) . f

updateVector :: (Double->Int) -> Double -> V.Vector Int -> V.Vector Int
updateVector f x v = v V.// [(idx, 1+(v V.! idx))]
    where
      idx = f x

pushEvery :: (Monad m) => Int -> Pipe a a m r
pushEvery n = pushEvery' n n

pushEvery' :: (Monad m) => Int -> Int -> Pipe a a m r
pushEvery' n 0 = await >>= yield >> pushEvery' n n
pushEvery' n m = await >> pushEvery' n (m-1)

histBuilder :: Monad m => (a -> Double) -> Int -> (Double,Double) -> Int -> Pipe a (V.Vector Int) m r
histBuilder f bins range delay = histBuilder' updater delay delay zeroList
    where
      zeroList = V.replicate bins 0
      updater = updateVector (toBin bins range) . f


histBuilder' :: Monad m => (a -> V.Vector Int -> V.Vector Int) -> Int -> Int -> V.Vector Int -> Pipe a (V.Vector Int) m r
histBuilder' f size 0 v = do
  event <- await
  let v2 = f event v
  yield v2
  histBuilder' f size size v2
histBuilder' f size n v = do
  event <- await
  let v2 = f event v
  histBuilder' f size (n-1) v2
