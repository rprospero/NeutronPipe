module Source () where

import Control.Monad (forever)
import Metropolis (metropolis)
import Pipes
import Neutron

generalSource p v = forever $ do
                      pos <- lift $ metropolis p
                      vel <- lift $ metropolis v
                      yield $ pos

test f = Neutron f f
