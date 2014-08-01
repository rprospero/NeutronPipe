module Main (main) where

import Control.Monad (forever)

import Neutron
import Vec
import Pipes

source :: Num a => Producer (Vec a) IO ()
source = forever $ yield 1

detector :: (Num a, Show a) => Consumer (Vec a) IO ()
detector = forever $ do
  temp <- await
  lift $ print temp

main = runEffect $ source >-> detector
