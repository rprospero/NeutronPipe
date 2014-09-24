{-# LANGUAGE FlexibleContexts #-}
module Source (simpleSource) where

import Control.Monad (forever)
import Pipes
import Neutron (Neutron(Neutron))
import Linear (normalize,V3,Epsilon,(*^))
import Data.Random (sample,sampleFrom,RVar,RandomSource)
import Momentum (Momentum(getSpeed),Energy,Speed,Wavelength)

{-# SPECIALIZE simpleSource :: (RandomSource IO s) => s -> RVar (V3 Double) -> RVar (V3 Double) -> RVar (Energy Double) -> Producer (Neutron Double) IO () #-}
simpleSource :: (RandomSource IO s, Epsilon a, Num a, Floating a, Momentum m) => s -> RVar (V3 a) -> RVar (V3 a) -> RVar (m a) -> Producer (Neutron a) IO ()
simpleSource src startArea targetArea momentumSpread = forever $
             do
               spread <- lift . sampleFrom src $ momentumSpread
               start <- lift . sampleFrom src $ startArea
               target <- lift . sampleFrom src $ targetArea
               yield . Neutron start 1 . (getSpeed spread *^). normalize $ (target-start)
