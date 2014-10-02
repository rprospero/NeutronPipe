module Metropolis (metropolis) where

import Data.Random

metropolis :: (Double -> Double) -> Int -> RVar Double
metropolis f count = last . take count $ scanl (>>=) (step 1) (repeat step)
    where
      step = met' f

met' :: (Double -> Double) -> Double -> RVar Double
met' f xold = do
  step <- uniform (-1) 1
  let original = f xold
  let xnew = xold + step
  let fnew = f xnew
  test <- uniform 0 1
  return $ if fnew/original > test
           then xnew
           else xold

