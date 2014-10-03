{-|
Module      : Main
Description : A simulator for neutron beamlines
Copyright   : (c) Adam Washington, 2014
License     : MIT
Maintainer  : adam.l.washington@gmail.com
Stability   : experimental
Portability : POSIX

This module performs a monte-carlo simulation of a neutron beamline.-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}
module Main (main) where

import Neutron (getEnergy,Neutron,position,advance)
import Momentum (Energy(Energy),Momentum)
import Pipes
import qualified Pipes.Prelude as P

import Slits (slit)
import Detector (dumpToConsole,liftBuilder)
import Source (simpleSource,producer)
import Samples (scatter,spheres)
import Control.Applicative
import Data.Random
import Data.Random.Source.PureMT
import Data.IORef
import Control.Comonad(extract)
import Linear (V3(V3),Epsilon,nearZero,norm)
import Control.Monad (liftM)

import Data.Vector.Unboxed as V
import Data.Random.Distribution.Uniform (doubleUniform)
import Data.Random.Distribution.Normal (doubleStdNormal)
--import Data.Random.Distribution (rVarT)

-- Step 1: Define the beamline
-- Make a beamline function with parameters for every random value

instance Num (V.Vector Double) where
    (+) = V.zipWith (+)
    (-) = V.zipWith (-)
    (*) = V.zipWith (*)
    abs = V.map abs
    signum = V.map signum
    fromInteger = V.replicate chunksize . fromInteger
instance Fractional (V.Vector Double) where
    (/) = V.zipWith (/)
    fromRational = V.replicate chunksize . fromRational
instance Floating (V.Vector Double) where
    sin = V.map sin
    cos = V.map cos
    log = V.map log
    exp = V.map exp
    asin = V.map asin
    atan = V.map atan
    acos = V.map acos
    sinh = V.map sinh
    cosh = V.map cosh
    asinh = V.map asinh
    acosh = V.map acosh
    atanh = V.map atanh
    pi = V.replicate chunksize pi
instance Epsilon (V.Vector Double) where
    nearZero = const False
instance Distribution Uniform (Vector Double) 
    where rvarT (Uniform a b) = V.zipWithM doubleUniform a b
instance Distribution Normal (Vector Double) 
    where 
      rvarT StdNormal = V.replicateM chunksize doubleStdNormal
      rvarT (Normal m s) = V.zipWith3 <$> pure (\ a b c -> a*b+c) <*> pure s <*> x <*> pure m
          where
            x = V.replicateM chunksize doubleStdNormal

chunksize :: Int
chunksize = 1000

startSlit :: Neutron (V.Vector Double) -> Maybe (Neutron (V.Vector Double))
startSlit = slit (V3 0 0 (-10)) (V3 0.4 0.9 10)

beamline :: (Momentum m) => V3 (V.Vector Double) -> V3 (V.Vector Double) -> m (V.Vector Double) -> V.Vector Double -> V.Vector Double -> Maybe (Neutron (V.Vector Double))
beamline start target momentum angle q = liftM (advance 1 . scatter q angle) . startSlit $ simpleSource start target momentum
--beamline = error "Fail"

-- Step 2: Define Random Variables
-- Make a random variable for each of the values

startbox :: RVar (V3 (V.Vector Double))
startbox = pure $ V3 0 0 0

targetbox :: RVar (V3 (V.Vector Double))
targetbox = pure $ V3 0 0 1

mySpread :: RVar (Energy (V.Vector Double))
mySpread = liftM Energy $ normal 1.0 0.5

-- Step 3: Make a random beamline
-- This should have NO parameters and a type of RVar (Maybe (Neutron Double))

beam :: RVar (Maybe (Neutron (V.Vector Double)))
beam = beamline <$> startbox <*> targetbox <*> mySpread <*> uniform 0 (2*pi) <*> spheres 10
--beam = error "Fail"

-- Step 4: Run the beamline!

x :: V3 a -> a
x (V3 n _ _) = n
y :: V3 a -> a
y (V3 _ n _) = n
z :: V3 a -> a
z (V3 _ _ n) = n

main' :: (RandomSource IO s) => s -> IO ()
-- | Simulate the beamline
main' src = runEffect $ producer src beam >->
            P.take 1000 >->
            liftBuilder (norm.position) 1000 (0,2) 999 >->
            dumpToConsole

main :: IO ()
main = do
  src <- newIORef (pureMT 1234)
  main' src
