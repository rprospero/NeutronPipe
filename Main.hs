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

import Neutron (getEnergy,Neutron(speed))
import Momentum (Energy(Energy),fromSpeed)
import Pipes
import qualified Pipes.Prelude as P

import Slits (slit)
import Detector (dumpToConsole,histBuilder2)
import Source (simpleSource)
import Control.Applicative
import Data.Random
import Data.Random.Source.PureMT
import Data.IORef
import Control.Comonad(extract)
import Control.Monad (liftM,liftM3)
import Linear (V3(V3))
import Linear.Epsilon
import Data.Foldable
import Data.Vector as U
import Data.Vector.Unboxed (Unbox)

instance (Num a, Unbox a) => Num (U.Vector a) where
    a + b = U.zipWith (+) a b
    a - b = U.zipWith (-) a b
    a * b = U.zipWith (*) a b
    abs = U.map abs
    signum = U.map signum
    fromInteger = U.replicate chunksize . fromInteger

instance (Fractional a,Unbox a) => Fractional (U.Vector a) where
    a / b = U.zipWith (/) a b
    fromRational = U.replicate chunksize . fromRational

instance (Floating a, Unbox a) => Floating (U.Vector a) where
    exp = U.map exp
    log = U.map log
    sin = U.map sin
    cos = U.map cos
    tan = U.map tan
    asin = U.map asin
    acos = U.map acos
    atan = U.map atan
    sinh = U.map sinh
    cosh = U.map cosh
    tanh = U.map atanh
    asinh = U.map asinh
    acosh = U.map acosh
    atanh = U.map atanh
    pi = U.replicate chunksize pi

instance (Unbox a, Epsilon a) => Epsilon (U.Vector a) where
     nearZero = const False

chunksize = 20

startbox :: RVar (V3 (U.Vector Double))
startbox = V3 <$> U.replicateM chunksize (uniform 0 1)
           <*> U.replicateM chunksize (uniform 0 1)
           <*> pure (U.replicate chunksize 0)

polarToCart :: (Floating a) => a -> a-> a -> V3 a
polarToCart rho phi d = V3 x0 y0 d
    where
      x0 = rho * cos phi
      y0 = rho * sin phi

-- targetbox :: RVar  (V3 (U.Vector Double))
-- targetbox = do
--   rho <- uniform 0 1
--   phi <- uniform 0 (2*pi)
--   let x0 = rho * cos phi
--   let y0 = rho * sin phi
--   return $ V3 (U.replicateM chunksize x0) (U.replicateM chunksize y0) (U.replicate chunksize 1)
targetbox :: RVar (V3 (U.Vector Double))
targetbox = liftM3 polarToCart rho phi d
    where
      rho = U.replicateM chunksize $ uniform 0 1
      phi = U.replicateM chunksize $ uniform 0 (2*pi)
      d = pure $ (U.replicate chunksize 0)

mySpread :: RVar (Energy (U.Vector Double))
mySpread = liftM Energy $ U.replicateM chunksize $  normal 1.0 0.5
           
--main' :: (RandomSource IO s) => s -> IO ()
-- | Simulate the beamline
main' src = runEffect $ simpleSource src startbox targetbox mySpread >-> 
            slit (V3 0 0 (-10)) (V3 0.4 0.9 10) >->
            P.take 1000000 >->
            histBuilder2 v w 40 (0,2) 50000 >->
            dumpToConsole
    where
      v :: Neutron (U.Vector a) -> U.Vector a
      v = speed
      w :: Double -> Double
      w x = extract (fromSpeed x :: Energy Double)

main :: IO ()
main = do
  src <- newIORef (pureMT 1234)
  main' src
