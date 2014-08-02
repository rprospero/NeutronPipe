{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Vec
Description : A small module for handling 3-Vectors
Copyright   : (c) Adam Washington, 2014
License     : MIT
Maintainer  : adam.l.washington@gmail.com
Stability   : experimental
Portability : POSIX

This module performs basic 3-vector math for handling simple gemoetry
in 3-space.  I'm sure that this will eventually be replaced by a
better module written by a more competent programmer.  -}

module Vec (Vec(Vec,x,y,z),dot,cross,scale) where

import Control.Applicative ((<*>))
import Data.List (intersperse)
import System.Random

-- | A data type for 3-vectors
data Vec a = Vec {x :: a, -- ^ x coordinate
                  y :: a, -- ^ y coordinate
                  z :: a  -- ^ z coordinate
                 }

instance Show a => Show (Vec a) where
    -- | Standard showing with "Vec" followed by the individual components
    show v = ("Vec " ++) . concat . intersperse " " . map show $ [x v, y v, z v]

instance Num a => Num (Vec a) where
    -- | Vector Elements are added componentwise
    (+) i j = Vec (x i + x j) (y i + y j) (z i + z j)
    -- | Same for subtraction
    (-) i j = Vec (x i - x j) (y i - y j) (z i - z j)
    -- | We'll go ahead and make multiplication the cross product, since it takes two vectors and returns a vector.
    (*) = cross
    -- | Unfortunately, abs has to return a vector with the absolute values of the members, instead of the actual vector normal, as we would want.
    abs i = Vec (abs (x i)) (abs (y i)) (abs (z i))
    -- | Returns a vector with all three coordinates set as the given integer.  This allows 0 :: Vec Int to be the null vector
    fromInteger i = Vec (fromInteger i) (fromInteger i) (fromInteger i)
    -- | The signum of the individual componenets.  I really have no idea how this would ever be useful.
    signum i = Vec (signum (x i)) (signum (y i)) (signum (z i))

-- | Returns the inner product of two vectors.  It might make sense to make an operator for this, but the obvious one is taken.
dot :: Num a => Vec a -> Vec a -> a
dot i j = x i * x j + y i * y j + z i * z j

-- | Gibb's cross product.  I need to test this to make sure that it's right handed
cross :: Num a => Vec a -> Vec a -> Vec a
cross i j = Vec (y i * z j - z i * y j)
                (z i * x j - x i * z j)
                (x i * y j - y i * x j)

-- | Scale a vector by a constant
scale :: Num a => a -> Vec a -> Vec a
scale s v = Vec (s * x v) (s * y v) (s * z v)


instance Random a => Random (Vec a) where
    -- | The components of the random vector will be between the components of the lo and hi vectors
    randomR (lo, hi) g =
        let (x',g1) = randomR (x lo, x hi) g
            (y',g2) = randomR (y lo, y hi) g1
            (z',g3) = randomR (z lo, z hi) g2
        in
          (Vec x' y' z',g3)
    -- | Generates a vector with random components
    random g =
        let (x',g1) = random g
            (y',g2) = random g1
            (z',g3) = random g2
        in
          (Vec x' y' z',g3)
