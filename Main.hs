module Main (main) where

import Control.Monad (forever)
import System.Random

import Neutron
import Vec
import Pipes

source :: Producer (Neutron Double) IO ()
source = forever $ do
           g <- lift getStdGen
           let (a,g2) = random g
           yield a
           lift $ setStdGen g2

detector :: (Num a, Show a) => Consumer (Neutron a) IO ()
detector = forever $ do
  temp <- await
  lift $ print temp

slit :: (Fractional a, Ord a) => Pipe (Neutron a) (Neutron a) IO ()
slit = forever $ do
         n <- await
         if (x . position $ n) < 0.5
         then
             yield n
         else
             discard n

main = runEffect $ source >-> slit >-> detector
