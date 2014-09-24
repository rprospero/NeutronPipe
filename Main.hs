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

import Neutron (getEnergy)
import Momentum (Energy(Energy),Momentum)
import Linear
import Pipes
import qualified Pipes.Prelude as P

import Slits (slit)
import Detector (dumpToConsole,histBuilder,pushEvery,histPipe)
import Source (simpleSource,Area(Rect,Circle))
import Control.Applicative
import Data.Random    
import Data.Traversable
import Control.Comonad(extract)

--startbox :: Area Double
--startbox = Rect 0 1 0 1
--targetbox :: Area Double
--targetbox = Circle 1

startbox :: RVar (Double, Double)
startbox = do
  x <- uniform 0 1
  y <- uniform 0 1
  return (x,y)

targetbox :: RVar (Double, Double)
targetbox = do
  rho <- uniform 0 1
  phi <- uniform 0 (2*pi)
  let x0 = rho * cos phi
  let y0 = rho * sin phi
  return (x0,y0)

uniformSpread :: Double -> Double -> RVar Double
uniformSpread a b = uniform (a-b) (a+b)

mySpread :: Energy (RVar Double) 
mySpread = normal <$> Energy 1.0 <*> Energy 0.1

main :: IO ()
-- | Simulate the beamline
main = runEffect $ simpleSource startbox targetbox 1 mySpread >-> 
       slit (V3 0 0 (-10)) (V3 0.4 0.9 10) >->
       P.take 100000 >->
--       histPipe (extract.getEnergy) 40 (0,2) >-> 
--       pushEvery 50000 >->
       histBuilder (extract.getEnergy) 40 (0,2) 50000 >->
       dumpToConsole
--       P.drain
