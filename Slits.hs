{-|
Module      : Slits
Description : A library of slits for neutron instruments
Copyright   : (c) Adam Washington, 2014
License     : MIT
Maintainer  : adam.l.washington@gmail.com
Stability   : experimental
Portability : POSIX

This module provides a library of slits for simulating a neutron beamline
-}

module Slits (slit) where

import Linear
import Neutron

neutronComp :: Ord a => V3 a -> V3 a -> Neutron a -> Bool
-- | Finds whether a neutron is between two other neutrons
neutronComp a b n = let p = position n
                    in (a < p) && (p < b)

slit :: Ord a => V3 a -> V3 a -> Neutron a -> Maybe (Neutron a)
-- | A pipe that only accepts neutrons with positions between the two parameters
slit lo hi n = if neutronComp lo hi n
               then Just n
               else Nothing
