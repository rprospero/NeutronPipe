{-# LANGUAGE FlexibleContexts #-}
module Metropolis (metropolis) where

import Data.Random

metropolis :: (Distribution Uniform a, Distribution Uniform b, Ord a, Num b, Fractional a) => (b -> a) -> Int -> RVar b
metropolis f count = last . take count $ scanl (>>=) (step 1) (repeat step)
    where
      step = met' f

met' :: (Distribution Uniform a, Distribution Uniform b, Ord a, Num b, Fractional a) => (b -> a) -> b -> RVar b
met' f xold = do
  step <- uniform (-1) 1
  let original = f xold
  let xnew = xold + step
  let fnew = f xnew
  test <- uniform 0 1
  return $ if fnew/original > test
           then xnew
           else xold
