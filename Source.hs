{-# LANGUAGE FlexibleContexts #-}
module Source (simpleSource) where

import Control.Monad (forever)
import Pipes
import Neutron (Neutron(Neutron))
import Linear (normalize,V3,Epsilon)
import Data.Random (sampleFrom,RVar,RandomSource)
import Momentum (Momentum(getSpeed))

{-# INLINABLE simpleSource #-}
simpleSource :: (RandomSource IO s, Epsilon a, Num a, Floating a, Momentum m) => s -> RVar (V3 a) -> RVar (V3 a) -> RVar (m a) -> Producer (Neutron a) IO ()
simpleSource src startArea targetArea momentumSpread = forever $
             do
               spread <- lift . sampleFrom src $ momentumSpread
               start <- lift . sampleFrom src $ startArea
               target <- lift . sampleFrom src $ targetArea
               yield $ Neutron start 1 (normalize (target-start)) (getSpeed spread)
