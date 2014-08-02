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

import Vec
import Neutron
import Pipes

-- | Determines whether all of the components of vector a are less
-- than the components of vector b
vecComp a b = and [x a < x b,
                   y a < y b,
                   z a < z b]

-- | Finds whether a neutron is between two other neutrons
neutronComp a b n = and [vecComp a (position n),
                         vecComp (position n) b]

-- | A pipe that takes a function f and only passes values that return true for f
filterPipe f = forever $ do
                 n <- await
                 if f n
                 then yield n
                 else discard n

-- | A pipe that only accepts neutrons with positions between the two parameters
slit lo hi = filterPipe (neutronComp lo hi)
