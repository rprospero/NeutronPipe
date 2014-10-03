module Samples (scatter) where

import Neutron
import Linear (V3(V3),normalize,cross,Epsilon,(*^))

scatter :: (Epsilon a, Floating a) => a -> a -> Neutron a -> Neutron a
scatter q angle n = n {direction = dir + q *^ (cos angle *^ v1 + sin angle *^ v2) }
    where
      dir = direction n
      v1 = normalize . cross dir $ V3 1 0 0
      v2 = normalize . cross dir $ V3 0 1 0
