module Source (simpleSource,Area(Circle,Rect)) where

import Control.Monad (forever)
import Pipes
import Neutron
import Vec
import Data.Random

data Area a = Circle a | Rect a a

inArea (Circle r) d = do
  rho <- sample $ uniform 0 r
  phi <- sample $ uniform 0 (2*pi)
  let x0 = rho * cos phi
  let y0 = rho * sin phi
  return (Vec x0 y0 d)
inArea (Rect h w) d = do
  x0 <- sample $ uniform (-w/2) (w/2)
  y0 <- sample $ uniform (-h/2) (h/2)
  return (Vec x0 y0 d)

simpleSource startArea targetArea distance speed dspeed= forever $
             do
               start <- lift $ inArea startArea 0
               target <- lift $ inArea targetArea distance
               spread <- lift . sample . randomMomentum $ dspeed
               let neutron = Neutron start 1 (target-start)
               yield $ setSpeed (speed+spread) neutron
