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

import Neutron (Momentum(Energy),rawMomentumValue,getEnergy)
import Vec (Vec(Vec))
import Pipes
import qualified Pipes.Prelude as P

import Slits (slit)
import Detector (dumpToConsole,histPipe)
import Source (simpleSource,Area(Rect,Circle))

startbox :: Area Double
startbox = Rect 1 1
targetbox :: Area Double
targetbox = Circle 1

main :: IO ()
-- | Simulate the beamline
main = runEffect $ simpleSource startbox targetbox 1 (Energy 1) >-> slit (Vec 0 0 (-10)) (Vec 0.4 0.9 10) >->
                   P.take 1000 >-> histPipe (rawMomentumValue.getEnergy) 40 (0,2) >-> dumpToConsole
