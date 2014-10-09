{-# LANGUAGE FlexibleContexts #-}
module Samples (scatter, spheres) where

import Neutron
import Linear (V3(V3),normalize,cross,Epsilon,(*^))
import Metropolis (arb)
import Data.Random

scatter :: (Epsilon a, Floating a) => a -> a -> Neutron a -> Neutron a
scatter q angle n = n {direction = dir + q *^ (cos angle *^ v1 + sin angle *^ v2) }
    where
      dir = direction n
      v1 = normalize . cross dir $ V3 1 0 0
      v2 = normalize . cross dir $ V3 0 1 0

spheres :: (Floating a, Distribution Uniform a, Ord a) => a -> RVar a
spheres r = do
  u <- arb spheres' (0,20.0) (1/9.0)
  return $ u/(2*r)

spheres' :: Floating a => a -> a
spheres' u = ((abs $ sin u - u * cos u)/u**3)**2
--spheres' u = exp $ -((abs $ 2-u)**2)

