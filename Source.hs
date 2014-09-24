{-# LANGUAGE FlexibleContexts #-}
module Source (simpleSource,Area(Circle,Rect)) where

import Control.Monad (forever)
import Pipes
import Neutron (Neutron(Neutron))
import Linear (normalize,V3(V3),Epsilon,(*^))
import Data.Random
import Momentum (Momentum(getSpeed),Energy,Speed,Wavelength)
import Data.Traversable

data Area a = Circle a | Rect a a a a

inArea :: (MonadRandom m, Floating a, Distribution Uniform a) => Area a -> a -> m (V3 a)
inArea (Circle r) d = do
  rho <- sample $ uniform 0 r
  phi <- sample $ uniform 0 (2*pi)
  let x0 = rho * cos phi
  let y0 = rho * sin phi
  return (V3 x0 y0 d)
inArea (Rect bottom top left right) d = do
  y0 <- sample $ uniform bottom top
  x0 <- sample $ uniform left right
  return (V3 x0 y0 d)

--samplePoint 

{-# SPECIALIZE simpleSource :: RVar (Double, Double) -> RVar (Double, Double) -> Double -> RVar (Energy Double) -> Producer (Neutron Double) IO () #-}
simpleSource :: (Epsilon a, Num a, Distribution Uniform a, Floating a, Momentum m) => RVar (a,a) -> RVar (a,a) -> a -> RVar (m a) -> Producer (Neutron a) IO ()
simpleSource startArea targetArea distance momentumSpread = forever $
             do
               spread <- lift . sample $ momentumSpread
               (x1,y1) <- lift . sample $ startArea
               (x2,y2) <- lift . sample $ targetArea
               let start = V3 x1 y1 0
               let target = V3 x2 y2 distance
               yield . Neutron start 1 . (getSpeed spread *^). normalize $ (target-start)
