module Neutron (Neutron,advance) where

import Data.List (intersperse)

import Vec

data Neutron a = Neutron {position :: Vec a,
                          velocity :: Vec a}

instance Show a => Show (Neutron a) where
    show n = ("Neutron " ++) . concat . intersperse " " . map show $ [position n, velocity n]

advance :: Num a => a -> Neutron a -> Neutron a
advance t n = n {position = position n + scale t (velocity n)}
