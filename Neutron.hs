module Neutron where

import Vec

data Neutron a = Neutron {position :: Vec a,
                          velocity :: Vec a}

advance :: Num a => a -> Neutron a -> Neutron a
advance t n = n {position = position n + scale t (velocity n)}
