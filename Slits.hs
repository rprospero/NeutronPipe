module Slits (slit) where

import Control.Monad (forever)

import Vec
import Neutron
import Pipes

vecComp a b = and [x a < x b,
                   y a < y b,
                   z a < z b]

neutronComp a b n = and [vecComp a (position n),
                         vecComp (position n) b]

filterPipe f = forever $ do
                 n <- await
                 if f n
                 then yield n
                 else discard n

slit a b = filterPipe (neutronComp a b)
