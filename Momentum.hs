{-# LANGUAGE FlexibleContexts #-}

module Momentum (Momentum(Momentum,Energy,Speed,Wavelength),
                rawMomentumValue, randomMomentum, getSpeed',
                getEnergy', getMomentum', getWavelength',
                toSpeed') where

import Control.Applicative
import Data.Random

data Momentum a = Speed a | Energy a | Wavelength a | Momentum a
                deriving (Eq,Ord,Show)

instance Functor Momentum where
    fmap f (Speed a) = Speed (f a)
    fmap f (Energy a) = Energy (f a)
    fmap f (Momentum a) = Momentum (f a)
    fmap f (Wavelength a) = Wavelength (f a)
instance Applicative Momentum where
    pure = Speed
    (<*>) (Speed f) (Speed a) = Speed (f a)
    (<*>) (Energy f) (Energy a) = Energy (f a)
    (<*>) (Momentum f) (Momentum a) = Momentum (f a)
    (<*>) (Wavelength f) (Wavelength a) = Wavelength (f a)
    
uniformSpread :: (Distribution Uniform a, Num a) => a -> RVar a
uniformSpread a = uniform (-a) a

randomMomentum :: (Distribution Uniform a, Num a) => Momentum a -> Momentum (RVar a)
randomMomentum = fmap uniformSpread
 
neutronMass :: Floating a => a
neutronMass = 1.67492735174e-27
planck :: Floating a => a
planck = 6.62606957e-34

toSpeed' :: Floating a => Momentum a -> a
toSpeed' (Speed s) = s
toSpeed' (Momentum m) = m/neutronMass
toSpeed' (Energy e) = sqrt (2*e/neutronMass)
toSpeed' (Wavelength l) = planck / l / neutronMass

getSpeed' :: a -> Momentum a
getSpeed' = Speed
getMomentum' :: Floating a => a -> Momentum a
getMomentum' = Momentum . (* neutronMass)
getEnergy' :: Floating a => a -> Momentum a
getEnergy' = Energy . (/ 2) . (* neutronMass) . (\s -> s*s)
getWavelength' :: Floating a => a -> Momentum a
getWavelength' = Wavelength . (planck /) . (* neutronMass)


rawMomentumValue :: Momentum a -> a
rawMomentumValue (Speed a) = a
rawMomentumValue (Energy a) = a
rawMomentumValue (Momentum a) = a
rawMomentumValue (Wavelength a) = a
