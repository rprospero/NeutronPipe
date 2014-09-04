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
module Neutron (Neutron(Neutron),advance,position,velocity,intensity,setSpeed,Momentum(Momentum,Wavelength,Energy,Speed),getSpeed,getMomentum,getEnergy,getWavelength,rawMomentumValue,randomMomentum) where

import Data.Random
import Control.Monad (liftM)

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

data Momentum a = Speed a | Energy a | Wavelength a | Momentum a
                deriving (Eq,Ord,Show)
instance (Num a) => Num (Momentum a) where
    (+) (Speed a) (Speed b) = Speed (a+b)
    (+) (Energy a) (Energy b) = Energy (a+b)
    (+) (Momentum a) (Momentum b) = Momentum (a+b)
    (+) (Wavelength a) (Wavelength b) = Wavelength (a+b)
    (-) (Speed a) (Speed b) = Speed (a-b)
    (-) (Energy a) (Energy b) = Energy (a-b)
    (-) (Momentum a) (Momentum b) = Momentum (a-b)
    (-) (Wavelength a) (Wavelength b) = Wavelength (a-b)
    (*) _ _ = error "Unphysical Value"
    abs (Speed a) = Speed (abs a)
    abs (Energy a) = Energy (abs a)
    abs (Momentum a) = Momentum (abs a)
    abs (Wavelength a) = Wavelength (abs a)
    signum (Speed a) = Speed (signum a)
    signum (Energy a) = Energy (signum a)
    signum (Momentum a) = Momentum (signum a)
    signum (Wavelength a) = Wavelength (signum a)
    fromInteger = Speed . fromInteger

randomMomentum (Speed a) = liftM Speed $ uniform (-a) a
randomMomentum (Momentum a) = liftM Momentum $ uniform (-a) a
randomMomentum (Wavelength a) = liftM Wavelength $ uniform (-a) a
randomMomentum (Energy a) = liftM Energy $ uniform (-a) a

neutronMass :: Floating a => a
neutronMass = 1.67492735174e-27
planck :: Floating a => a
planck = 6.62606957e-34

toSpeed' :: Floating a => Momentum a -> a
toSpeed' (Speed s) = s
toSpeed' (Momentum m) = m/neutronMass
toSpeed' (Energy e) = sqrt (2*e/neutronMass)
toSpeed' (Wavelength l) = planck / l / neutronMass

setSpeed :: Floating a => Momentum a -> Neutron a -> Neutron a
setSpeed s n = n {velocity = rescale (toSpeed' s) $ velocity n}


getSpeed' :: a -> Momentum a
getSpeed' = Speed
getMomentum' :: Floating a => a -> Momentum a
getMomentum' = Momentum . (* neutronMass)
getEnergy' :: Floating a => a -> Momentum a
getEnergy' = Energy . (/ 2) . (* neutronMass) . (\s -> s*s)
getWavelength' :: Floating a => a -> Momentum a
getWavelength' = Wavelength . (planck /) . (* neutronMass)


getSpeed :: Floating a => Neutron a -> Momentum a
getSpeed = getSpeed' . norm . velocity
getMomentum :: Floating a => Neutron a -> Momentum a
getMomentum = getMomentum' .  norm . velocity
getEnergy :: Floating a => Neutron a -> Momentum a
getEnergy = getEnergy' . norm . velocity
getWavelength :: Floating a => Neutron a -> Momentum a
getWavelength = getWavelength' . norm . velocity

rawMomentumValue :: Momentum a -> a
rawMomentumValue (Speed a) = a
rawMomentumValue (Energy a) = a
rawMomentumValue (Momentum a) = a
rawMomentumValue (Wavelength a) = a
