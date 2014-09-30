{-|
Module      : Main
Description : A simulator for neutron beamlines
Copyright   : (c) Adam Washington, 2014
License     : MIT
Maintainer  : adam.l.washington@gmail.com
Stability   : experimental
Portability : POSIX

This module performs a monte-carlo simulation of a neutron beamline.-}
{-# LANGUAGE FlexibleContexts #-}
module Main (main) where

import Neutron (getEnergy,Neutron)
import Momentum (Energy(Energy),Momentum)
import Pipes
import qualified Pipes.Prelude as P

import Slits (slit)
import Detector (dumpToConsole,histBuilder)
import Source (simpleSource,producer)
import Control.Applicative
import Data.Random
import Data.Random.Source.PureMT
import Data.IORef
import Control.Comonad(extract)
import Linear (V3(V3))
import Control.Monad (liftM)

-- Step 1: Define the beamline
-- Make a beamline function with parameters for every random value

startSlit :: Neutron Double -> Maybe (Neutron Double)
startSlit = slit (V3 0 0 (-10)) (V3 0.4 0.9 10)

beamline :: (Momentum m) => V3 Double -> V3 Double -> m Double -> Maybe (Neutron Double)
beamline start target momentum = startSlit $ simpleSource start target momentum

-- Step 2: Define Random Variables
-- Make a random variable for each of the values

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

-- Step 3: Make a random beamline
-- This should have NO parameters and a type of RVar (Maybe (Neutron Double))

beam :: RVar (Maybe (Neutron Double))
beam = beamline <$> startbox <*> targetbox <*> mySpread

-- Step 4: Run the beamline!

main' :: (RandomSource IO s) => s -> IO ()
-- | Simulate the beamline
main' src = runEffect $ producer src beam >->
            P.take 1000000 >->
            histBuilder (extract.getEnergy) 40 (0,2) 50000 >->
            dumpToConsole

main :: IO ()
main = do
  src <- newIORef (pureMT 1234)
  main' src
