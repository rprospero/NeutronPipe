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
module Neutron (Neutron(Neutron),advance,position,intensity,setSpeed,getSpeed,getEnergy,getWavelength,speed,direction) where

import Linear
import qualified Momentum as M

-- | Holds the data for a simulate neutron trajectory
data Neutron a = Neutron {position :: V3 a, -- ^ The location of the neutron
                          intensity :: a, -- ^ The intensity of this neutron path
                          direction :: V3 a,  -- ^ The direction of the neutron's motion
                          speed :: a -- ^ The Speed of the neutron
                         }
               deriving (Eq,Show)

advance :: (Epsilon a, Floating a, Num a) => a -> Neutron a -> Neutron a
-- ^ Progress forward in time, allowing a neutron to follow its velocity.
-- ^ The first argument is the time which has passed.
advance t n = n {position = position n + (speed n * t) *^ (normalize $ direction n)}


getSpeed :: Floating a => Neutron a -> M.Speed a
getSpeed = M.fromSpeed . speed
getEnergy :: Floating a => Neutron a -> M.Energy a
getEnergy = M.fromSpeed . speed
getWavelength :: Floating a => Neutron a -> M.Wavelength a
getWavelength = M.fromSpeed . speed

setSpeed :: (M.Momentum m, Floating a, Epsilon a) => m a -> Neutron a -> Neutron a
setSpeed s n = n {speed = M.getSpeed s}

