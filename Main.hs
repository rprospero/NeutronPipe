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

import Neutron (Momentum(Speed,Energy,Momentum,Wavelength),getEnergy)
import Momentum (rawMomentumValue)
import Vec (Vec(Vec))
import Pipes
import qualified Pipes.Prelude as P

import Slits (slit)
import Detector (dumpToConsole,histPipe,pushEvery)
import Source (simpleSource,Area(Rect,Circle))
import Control.Monad (liftM)

startbox :: Area Double
startbox = Rect 1 1
targetbox :: Area Double
targetbox = Circle 1

uniformSpread :: Double -> Double -> RVar Double
uniformSpread a b = uniform (a-b) (a+b)

mSpread :: (Double -> Double -> RVar Double) -> Momentum Double -> Momentum Double -> RVar (Momentum Double)
mSpread f (Speed center) (Speed spread) = liftM Speed $ f center spread
mSpread f (Energy center) (Energy spread) = liftM Energy $ f center spread
mSpread f (Momentum center) (Momentum spread) = liftM Momentum $ f center spread
mSpread f (Wavelength center) (Wavelength spread) = liftM Wavelength $ f center spread

mySpread :: RVar (Momentum Double)
mySpread = mSpread normal (Energy 1.0) (Energy 0.1)

main :: IO ()
-- | Simulate the beamline
main = runEffect $ simpleSource startbox targetbox 1 mySpread >-> 
       slit (Vec 0 0 (-10)) (Vec 0.4 0.9 10) >->
       P.take 10000 >-> 
       histPipe (rawMomentumValue.getEnergy) 40 (0,2) >-> 
       pushEvery 2000 >->
       dumpToConsole
