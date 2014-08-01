module Main (main) where

import Control.Monad (forever)
import System.Random

import Neutron
import Vec
import Pipes

import Slits (slit)

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

main = runEffect $ source >-> slit (Vec 0.2 0.7 (-10)) (Vec 0.3 0.9 10) >-> detector
