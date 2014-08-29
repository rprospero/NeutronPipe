module Source (source) where

import Control.Monad (forever)
import Metropolis (metropolis)
import Pipes
import Neutron
import System.Random

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
