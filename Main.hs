module Main (main) where

import Control.Monad (forever)
import System.Random

import Neutron
import Vec
import Pipes

source :: Producer (Vec Double) IO ()
source = forever $ do
           g <- lift getStdGen
           let (a,g2) = random g
           yield a
           lift $ setStdGen g2

detector :: (Num a, Show a) => Consumer (Vec a) IO ()
detector = forever $ do
  temp <- await
  lift $ print temp

main = runEffect $ source >-> detector
