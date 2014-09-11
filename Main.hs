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
import Linear
import Pipes
import qualified Pipes.Prelude as P

import Slits (slit)
import Detector (dumpToConsole,histPipe,pushEvery)
import Source (simpleSource,Area(Rect,Circle))
import Control.Applicative
import Data.Random    
import Data.Traversable
import Control.Comonad(extract)

startbox :: Area Double
startbox = Rect 0 1 0 1
targetbox :: Area Double
targetbox = Circle 1

uniformSpread :: Double -> Double -> RVar Double
uniformSpread a b = uniform (a-b) (a+b)

pair :: a -> b -> (a,b)
pair a b = (a,b)

mSpread :: (Double -> Double -> RVar Double) -> Momentum Double -> Momentum Double -> RVar (Momentum Double)
mSpread f center spread = traverse (uncurry f) $ fmap pair center <*> spread

mySpread :: RVar (Momentum Double)
mySpread = mSpread normal (Energy 1.0) (Energy 0.1)

main :: IO ()
-- | Simulate the beamline
main = runEffect $ simpleSource startbox targetbox 1 mySpread >-> 
       slit (V3 0 0 (-10)) (V3 0.4 0.9 10) >->
       P.take 1000000 >-> 
       histPipe (extract.getEnergy) 40 (0,2) >-> 
       pushEvery 500000 >->
       dumpToConsole
