module Source (source) where

import Control.Monad (forever)
import Pipes
import Neutron
import Vec
import Haste

source :: Producer (Neutron Double) IO ()
source = forever $ do
           seed <- lift newSeed
           let (a,_) = randomR (Neutron (Vec 0 0 0) 0 (Vec 0 0 0),
                                 Neutron (Vec 1 1 1) 1 (Vec 1 1 1)) seed
           yield a

data Area a = Circle a | Rect a a

-- inArea :: MonadRandom m => Area Double -> m (Vec Double)
-- inArea (Circle r) = do
--   rho <- getRandom
--   phi <- getRandom
--   let x0 = r * rho * cos (2*pi*phi)
--   let y0 = r * rho * sin (2*pi*phi)
--   return (Vec x0 y0 0)

-- simpleSource startArea targetArea distance = forever $
--                                              do
--                                                start <- lift $ inArea startArea
--                                                target <- lift $ inArea targetArea
--                                                yield (Neutron start 1 target)
