{-# LANGUAGE FlexibleContexts #-}
module Source (simpleSource,producer) where

import Neutron (Neutron(Neutron))
import Linear (normalize,V3,Epsilon)
import Momentum (Momentum(getSpeed))

import Pipes
import Control.Monad (forever)
import Data.Random

{-# INLINABLE simpleSource #-}
simpleSource :: (Epsilon a, Momentum m, Floating a) => V3 a -> V3 a -> m a -> Neutron a
simpleSource start target momentum =
    Neutron start 1 (normalize (target - start)) (getSpeed momentum)

producer :: (RandomSource IO s) => s -> RVar (Maybe a) -> Producer a IO ()
producer src beam = forever $ do
  n <- lift $ runRVar beam src
  case n of
    Just neutron -> yield neutron
    Nothing -> discard n
