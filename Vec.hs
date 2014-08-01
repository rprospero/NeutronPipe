{-# LANGUAGE OverloadedStrings #-}
module Vec (Vec,dot,cross,scale,x,y,z) where

import Control.Applicative ((<*>))
import Data.List (intersperse)
import System.Random

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

instance Random a => Random (Vec a) where
    randomR (lo, hi) g =
        let (x',g1) = randomR (x lo, x hi) g
            (y',g2) = randomR (y lo, y hi) g1
            (z',g3) = randomR (z lo, z hi) g2
        in
          (Vec x' y' z',g3)
    random g =
        let (x',g1) = random g
            (y',g2) = random g1
            (z',g3) = random g2
        in
          (Vec x' y' z',g3)
