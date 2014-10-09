{-# LANGUAGE FlexibleContexts #-}
module Metropolis (arb) where

import Data.Random

arb :: (Distribution Uniform a, Distribution Uniform b, Ord a, Num b, Ord b) => (a->b) -> (a,a) -> b -> RVar a
arb f (low, high) top = do
  site <- uniform low high
  test <- uniform 0 top
  if f site > test
  then return site
  else arb f (low, high) top
