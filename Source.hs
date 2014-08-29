module Source (source,simpleSource) where

import Control.Monad (forever)
import Metropolis (metropolis)
import Pipes
import Neutron
import Vec
import System.Random
import Control.Monad.Random.Class

generalSource p v = forever $ do
                      pos <- lift $ metropolis p
                      vel <- lift $ metropolis v
                      yield $ pos

source :: Producer (Neutron Double) IO ()
source = forever $ do
           g <- lift getStdGen
           let (a,g2) = random g
           yield a
           lift $ setStdGen g2

data Area a = Circle a | Rect a a

inArea :: MonadRandom m => Area Double -> m (Vec Double)
inArea (Circle r) = do
  rho <- getRandom
  phi <- getRandom
  let x0 = r * rho * cos (2*pi*phi)
  let y0 = r * rho * sin (2*pi*phi)
  return (Vec x0 y0 0)

simpleSource startArea targetArea distance = forever $
                                             do
                                               start <- lift $ inArea startArea
                                               target <- lift $ inArea targetArea
                                               yield (Neutron start 1 target)
