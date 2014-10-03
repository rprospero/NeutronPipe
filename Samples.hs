module Samples (scatter) where

import Neutron
import Linear.V3

scatter :: Floating a => a -> Neutron a -> Neutron a
scatter angle n = n {direction = V3 (cos angle) (sin angle) 0 + direction n}
