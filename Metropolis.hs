{-# LANGUAGE FlexibleContexts #-}
module Metropolis (metropolis,met) where

import Data.Random

metropolis :: (Distribution Uniform a, Distribution Uniform b, Ord a, Num b, Fractional a) => (b -> a) -> Int -> RVar b
metropolis f count = last . take count $ scanl (>>=) (step 0) (repeat step)
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
 
met :: (Ord a, Fractional a, Distribution Uniform a) => (a -> a) -> Int -> RVar a
met f count = last . take count . iterate looper $ uniform 0 5
    where
      looper = (mettest f =<<)

mettest :: (Ord a, Fractional a, Distribution Uniform a) => (a -> a) -> a -> RVar a
mettest f old = do
  let yold = f old
  step <- uniform (-0.01) 0.01
  let xnew = old + step
  let ynew = f xnew
  test <- uniform 0 1
  return $ if ynew/yold > test
           then xnew
           else old
