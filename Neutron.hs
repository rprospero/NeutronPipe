{-|
Module      : Neutron
Description : Performs a classical simulation of a single neutron
Copyright   : (c) Adam Washington, 2014
License     : MIT
Maintainer  : adam.l.washington@gmail.com
Stability   : experimental
Portability : POSIX

This module allows for simulating a neutron trajectory in a classical way.

-}
module Neutron (Neutron(Neutron),advance,position,velocity,intensity) where

--import           System.Random
import Haste (Random,randomR)

import           Vec

-- | Holds the data for a simulate neutron trajectory
data Neutron a = Neutron {position :: Vec a, -- ^ The location of the neutron
                          intensity :: a, -- ^ The intensity of this neutron path
                          velocity :: Vec a  -- ^ The direction and speed of the neutron's motion
                         }

instance Show a => Show (Neutron a) where
    show n = ("Neutron " ++) . unwords . map show $ [position n, velocity n]

advance :: Num a => a -> Neutron a -> Neutron a
-- ^ Progress forward in time, allowing a neutron to follow its velocity.
-- ^ The first argument is the time which has passed.
advance t n = n {position = position n + scale t (velocity n)}

instance (Num a, Random a) => Random (Neutron a) where
    randomR (lo, hi) g =
        let (p, g1) = randomR (position lo, position hi) g
            (v, g2) = randomR (velocity lo, velocity hi) g1
        in
          (Neutron p 1 v, g2)
