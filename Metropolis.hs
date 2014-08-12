module Metropolis (metropolis) where

import System.Random
import Control.Monad.Random.Class

norm x = exp (-1 * x**2)

metropolis
  :: (MonadRandom m, Random a, Random b, Ord a, Num b,
      Fractional a) =>
     (b -> a) -> m [b]
metropolis f = do
  base <- getRandom
  steps <- getRandoms
  tests <- getRandoms
  return $ scanl (met' f) base $ zip steps tests

met'
  :: (Ord a, Num a1, Fractional a) =>
     (a1 -> a) -> a1 -> (a1, a) -> a1
met' f xold (xstep,test) = let yold = f xold
                               ynew = f $ xold + xstep
                           in
                             if ynew/yold > test
                             then
                                 xold + xstep
                             else
                                 xold
