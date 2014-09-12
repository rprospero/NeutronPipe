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
module Neutron (Neutron(Neutron),advance,position,velocity,intensity,setSpeed,getSpeed,getEnergy,getWavelength) where

import Linear
import qualified Momentum as M

-- | Holds the data for a simulate neutron trajectory
data Neutron a = Neutron {position :: V3 a, -- ^ The location of the neutron
                          intensity :: a, -- ^ The intensity of this neutron path
                          velocity :: V3 a  -- ^ The direction and speed of the neutron's motion
                         }

instance Show a => Show (Neutron a) where
    show n = ("Neutron " ++) . unwords . map show $ [position n, velocity n]

advance :: Num a => a -> Neutron a -> Neutron a
-- ^ Progress forward in time, allowing a neutron to follow its velocity.
-- ^ The first argument is the time which has passed.
advance t n = n {position = position n + t *^ (velocity n)}


getSpeed :: Floating a => Neutron a -> M.Speed a
getSpeed = M.fromSpeed . norm . velocity
getEnergy :: Floating a => Neutron a -> M.Energy a
getEnergy = M.fromSpeed . norm . velocity
getWavelength :: Floating a => Neutron a -> M.Wavelength a
getWavelength = M.fromSpeed . norm . velocity

setSpeed :: (M.Momentum m, Floating a, Epsilon a) => m a -> Neutron a -> Neutron a
setSpeed s n = n {velocity = (M.getSpeed s *^) . normalize $ velocity n}

