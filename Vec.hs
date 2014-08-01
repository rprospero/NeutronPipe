{-# LANGUAGE OverloadedStrings #-}
module Vec (Vec,dot,cross,scale) where

import Control.Applicative ((<*>))
import Data.List (intersperse)

data Vec a = Vec {x :: a, y :: a, z :: a}

instance Show a => Show (Vec a) where
    show v = ("Vec " ++) . concat . intersperse " " . map show $ [x v, y v, z v]

instance Num a => Num (Vec a) where
    (+) i j = Vec (x i + x j) (y i + y j) (z i + z j)
    (-) i j = Vec (x i - x j) (y i - y j) (z i - z j)
    (*) = cross
    abs i = Vec (abs (x i)) (abs (y i)) (abs (z i))
    fromInteger i = Vec (fromInteger i) (fromInteger i) (fromInteger i)
    signum i = Vec (signum (x i)) (signum (y i)) (signum (z i))

dot :: Num a => Vec a -> Vec a -> a
dot i j = x i * x j + y i * y j + z i * z j

cross :: Num a => Vec a -> Vec a -> Vec a
cross i j = Vec (y i * z j - z i * y j)
                (z i * x j - x i * z j)
                (x i * y j - y i * x j)

scale :: Num a => a -> Vec a -> Vec a
scale s v = Vec (s * x v) (s * y v) (s * z v)
