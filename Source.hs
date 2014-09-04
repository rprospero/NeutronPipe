module Source (source,simpleSource,Area(Circle,Rect)) where

import Control.Monad (forever)
import Metropolis (metropolis)
import Pipes
import Neutron
import Vec
import System.Random
import Control.Monad.Random.Class

source :: Producer (Neutron Double) IO ()
source = forever $ do
           g <- lift getStdGen
           let (a,g2) = random g
           yield a
           lift $ setStdGen g2

data Area a = Circle a | Rect a a

inArea :: (MonadRandom m, Floating a, Random a) => Area a -> a -> m (Vec a)
inArea (Circle r) d = do
  rho <- getRandom
  phi <- getRandom
  let x0 = r * rho * cos (2*pi*phi)
  let y0 = r * rho * sin (2*pi*phi)
  return (Vec x0 y0 d)
inArea (Rect h w) d = do
  x <- getRandom
  y <- getRandom
  let x0 = w * (x-0.5)
  let y0 = h * (y-0.5)
  return (Vec x0 y0 d)

simpleSource :: (Floating a, Random a) => Area a -> Area a -> a -> Momentum a -> Momentum a -> Producer (Neutron a) IO ()
simpleSource startArea targetArea distance speed dspeed= forever $
             do
               start <- lift $ inArea startArea 0
               target <- lift $ inArea targetArea distance
               spread <- lift $ getRandomMomentum dspeed
               let neutron = Neutron start 1 (target-start)
               yield $ setSpeed (speed+spread) neutron

instance MonadRandom IO where
    getRandom = randomIO
    getRandomR = randomRIO
    getRandoms = fmap randoms newStdGen
    getRandomRs b = fmap (randomRs b) newStdGen
