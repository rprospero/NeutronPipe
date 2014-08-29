{-|
Module      : Main
Description : A simulator for neutron beamlines
Copyright   : (c) Adam Washington, 2014
License     : MIT
Maintainer  : adam.l.washington@gmail.com
Stability   : experimental
Portability : POSIX

This module performs a monte-carlo simulation of a neutron beamline.-}
module Main (main) where

import Control.Monad (forever)
--import System.Random
import Haste (randomR,newSeed)

import Neutron
import Vec
import Pipes
import qualified Pipes.Lift as PL
import qualified Pipes.Prelude as P

import Slits (slit)
import Detector (dumpToFile)

source :: Producer (Neutron Double) IO ()
source = forever $ do
           g <- newSeed
           let (a,_) = randomR (Neutron (Vec 0 0 0) (Vec 0 0 0),
                                         Neutron (Vec 1 1 1) (Vec 1 1 1)) g
           yield a


-- | Simulate the beamline
main = runEffect $ source >-> slit (Vec 0.2 0.7 (-10)) (Vec 0.3 0.9 10) >-> P.take 1000 >-> dumpToFile "test2.dat"

-- main = def & plot_bars_style .~ BarsStacked
--       & plot_bars_item_styles .~
--           [ (FillStyleSolid $ withOpacity (sRGB24 255 0 0) 100, Nothing)
--           , (FillStyleSolid $ withOpacity (sRGB24 0 255 0) 100, Nothing)
--           ]
--       & plot_bars_titles .~ [ "Downvotes", "Upvotes" ]
--       & plot_bars_values .~ dataPoints
--  where
--   dataPoints =
--    zip [1..] $ map (sequence [blogPostDownvotes, blogPostUpvotes]) blogPosts
