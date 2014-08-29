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

vecComp :: Ord a => Vec a -> Vec a -> Bool
-- | Determines whether all of the components of vector a are less
-- than the components of vector b
vecComp a b = x a < x b && y a < y b && z a < z b

neutronComp :: Ord a => Vec a -> Vec a -> Neutron a -> Bool
-- | Finds whether a neutron is between two other neutrons
neutronComp a b n = let p = position n
                    in vecComp a p && vecComp p b

filterPipe :: Monad m => (a->Bool) -> Pipe a a m b                      
-- | A pipe that takes a function f and only passes values that return true for f
filterPipe f = forever $ do
                 n <- await
                 if f n
                 then yield n
                 else discard n

slit :: (Ord a , Monad m) => Vec a -> Vec a -> Pipe (Neutron a) (Neutron a) m b
-- | A pipe that only accepts neutrons with positions between the two parameters
slit lo hi = filterPipe (neutronComp lo hi)
