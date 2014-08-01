module Neutron (Neutron,advance,position,velocity) where

import Data.List (intersperse)
import System.Random

import Vec

data Neutron a = Neutron {position :: Vec a,
                          velocity :: Vec a}

instance Show a => Show (Neutron a) where
    show n = ("Neutron " ++) . concat . intersperse " " . map show $ [position n, velocity n]

advance :: Num a => a -> Neutron a -> Neutron a
advance t n = n {position = position n + scale t (velocity n)}

instance Random a => Random (Neutron a) where
    randomR (lo, hi) g =
        let (p, g1) = randomR (position lo, position hi) g
            (v, g2) = randomR (velocity lo, velocity hi) g1
        in
          (Neutron p v, g2)
    random g =
        let (p, g1) = random g
            (v, g2) = random g1
        in
          (Neutron p v, g2)
