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
import Momentum (Energy(Energy))
import Pipes
import qualified Pipes.Prelude as P

import Slits (slit)
import Detector (dumpToConsole,histBuilder)
import Source (simpleSource)
import Control.Applicative
import Data.Random
import Data.Random.Source.PureMT
import Data.IORef
import Control.Comonad(extract)
import Control.Monad (liftM)
import Linear (V3(V3))

startbox :: RVar (V3 Double)
startbox = V3 <$> uniform 0 1 <*> uniform 0 1 <*> pure 0

targetbox :: RVar (V3 Double)
targetbox = do
  rho <- uniform 0 1
  phi <- uniform 0 (2*pi)
  let x0 = rho * cos phi
  let y0 = rho * sin phi
  return $ V3 x0 y0 1

mySpread :: RVar (Energy Double)
mySpread = liftM Energy $ normal 1.0 0.5
           
--main' :: (RandomSource IO s) => s -> IO ()
-- | Simulate the beamline
main' src = runEffect $ simpleSource src startbox targetbox mySpread >-> 
            slit (V3 0 0 (-10)) (V3 0.4 0.9 10) >->
            P.take 1000000 >->
            histBuilder (extract.getEnergy) 40 (0,2) 50000 >->
            dumpToConsole

main :: IO ()
main = do
  src <- newIORef (pureMT 1234)
  main' src
