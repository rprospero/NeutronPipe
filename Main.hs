module Main (main) where

import Control.Monad (forever)
--import System.Random
import Haste (randomR,newSeed)

import Neutron
import Vec
import Pipes

import Slits (slit)

source :: Producer (Neutron Double) IO ()
source = forever $ do
           g <- newSeed
           let (a,_) = randomR (Neutron (Vec 0 0 0) (Vec 0 0 0),
                                         Neutron (Vec 1 1 1) (Vec 1 1 1)) g
           yield a

detector :: (Num a, Show a) => Consumer (Neutron a) IO ()
detector = forever $ do
  temp <- await
  lift $ print temp

main = runEffect $ source >-> slit (Vec 0.2 0.7 (-10)) (Vec 0.3 0.9 10) >-> detector
