{-# LANGUAGE FlexibleContexts #-}

module Momentum (Speed(Speed),Energy(Energy),Wavelength(Wavelength),
                 Momentum(getSpeed,fromSpeed)) where

import Control.Applicative
import Control.Comonad
import Data.Random
import Data.Traversable
import Data.Foldable

--data Momentum a = Speed a | Energy a | Wavelength a | Momentum a
--                deriving (Eq,Ord,Show)

class Momentum m where
    getSpeed :: Floating a => m a -> a
    fromSpeed :: Floating a => a -> m a

newtype Speed a = Speed a deriving (Show,Eq,Ord)

instance Functor Speed where
    fmap f (Speed a) = Speed (f a)
instance Applicative Speed where
    pure = Speed
    (<*>) (Speed f) (Speed a) = Speed (f a)
instance Foldable Speed where
    foldr f initial (Speed a) = f a initial
instance Traversable Speed where
    traverse f (Speed n) = liftA Speed $ f n
instance Comonad Speed where
    extract (Speed a) = a
    duplicate (Speed a) = Speed (Speed a)
instance Momentum Speed where
    getSpeed = extract
    fromSpeed = Speed

newtype Energy a = Energy a


instance Functor Energy where
    fmap f (Energy a) = Energy (f a)
instance Applicative Energy where
    pure = Energy
    (<*>) (Energy f) (Energy a) = Energy (f a)
instance Foldable Energy where
    foldr f initial (Energy a) = f a initial
instance Traversable Energy where
    traverse f (Energy n) = liftA Energy $ f n
instance Comonad Energy where
    extract (Energy a) = a
    duplicate (Energy a) = Energy (Energy a)
instance Momentum Energy where
    getSpeed = (\e -> sqrt (2*e/neutronMass)) . extract
    fromSpeed = Energy . (\s -> s*s*neutronMass/2)

newtype Wavelength a = Wavelength a

instance Functor Wavelength where
    fmap f (Wavelength a) = Wavelength (f a)
instance Applicative Wavelength where
    pure = Wavelength
    (<*>) (Wavelength f) (Wavelength a) = Wavelength (f a)
instance Foldable Wavelength where
    foldr f initial (Wavelength a) = f a initial
instance Traversable Wavelength where
    traverse f (Wavelength n) = liftA Wavelength $ f n
instance Comonad Wavelength where
    extract (Wavelength a) = a
    duplicate (Wavelength a) = Wavelength (Wavelength a)
instance Momentum Wavelength where
    getSpeed = (\a -> planck / a / neutronMass) . extract
    fromSpeed = Wavelength . (\s -> planck / s / neutronMass)

uniformSpread :: (Distribution Uniform a, Num a) => a -> RVar a
uniformSpread a = uniform (-a) a

neutronMass :: Floating a => a
{-# INLINE neutronMass #-}
neutronMass = 1.67492735174e-27
planck :: Floating a => a
{-# INLINE planck #-}
planck = 6.62606957e-34
