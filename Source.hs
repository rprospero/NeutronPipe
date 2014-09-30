{-# LANGUAGE FlexibleContexts #-}
module Source (simpleSource) where

import Neutron (Neutron(Neutron))
import Linear (normalize,V3,Epsilon)
import Momentum (Momentum(getSpeed))

{-# INLINABLE simpleSource #-}
simpleSource :: (Epsilon a, Momentum m, Floating a) => V3 a -> V3 a -> m a -> Neutron a
simpleSource start target momentum =
    Neutron start 1 (normalize (target - start)) (getSpeed momentum)
