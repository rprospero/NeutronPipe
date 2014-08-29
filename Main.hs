{-|
Module      : Main
Description : A simulator for neutron beamlines
Copyright   : (c) Adam Washington, 2014
License     : MIT
Maintainer  : adam.l.washington@gmail.com
Stability   : experimental
Portability : POSIX

This module performs a monte-carlo simulation of a neutron beamline.-}
module Main (main) where

import Control.Monad (forever)
--import System.Random
import Haste (randomR,newSeed)

import Neutron
import Vec
import Pipes
import qualified Pipes.Prelude as P

import Slits (slit)
import Detector (histPipe)
import Detector.Haste
import Source (source)

main :: IO ()
-- | Simulate the beamline
main = runEffect $ source >-> slit (Vec 0.2 0.7 (-10)) (Vec 0.4 0.9 10) >->
                   P.take 1000 >-> histPipe (x.position) 40 (0,1) >-> plotHists
