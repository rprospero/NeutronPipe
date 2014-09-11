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

import Control.Monad (forever)

import Linear
import Neutron
import Pipes
import qualified Pipes.Prelude as P

vecComp :: Ord a => V3 a -> V3 a -> Bool
-- | Determines whether all of the components of vector a are less
-- than the components of vector b
vecComp a b = a < b

neutronComp :: Ord a => V3 a -> V3 a -> Neutron a -> Bool
-- | Finds whether a neutron is between two other neutrons
neutronComp a b n = let p = position n
                    in vecComp a p && vecComp p b

filterPipe :: Monad m => (a->Bool) -> Pipe a a m b                      
-- | A pipe that takes a function f and only passes values that return true for f
filterPipe = P.filter

slit :: (Ord a , Monad m) => V3 a -> V3 a -> Pipe (Neutron a) (Neutron a) m b
-- | A pipe that only accepts neutrons with positions between the two parameters
slit lo hi = filterPipe (neutronComp lo hi)
