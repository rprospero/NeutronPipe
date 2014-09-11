{-# LANGUAGE FlexibleContexts #-}
module Source (simpleSource,Area(Circle,Rect)) where

import Control.Monad (forever)
import Pipes
import Neutron
import Linear
import Data.Random

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

simpleSource :: (Epsilon a, Num a, Distribution Uniform a, Floating a) => Area a -> Area a -> a -> RVar (Momentum a) -> Producer (Neutron a) IO ()
simpleSource startArea targetArea distance momentumSpread = forever $
             do
               start <- lift $ inArea startArea 0
               target <- lift $ inArea targetArea distance
               spread <- lift . sample $ momentumSpread
               let neutron = Neutron start 1 (target-start)
               yield $ setSpeed spread neutron
