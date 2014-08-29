module Detector.Haste (plotHists) where

import Pipes
import Control.Monad (forever,zipWithM_)
    
import Haste.Graphics.Canvas
import Haste


plotHists :: Consumer [Int] IO ()
plotHists = forever $ do
              Just c <- getCanvasById "canvas"
              ys <- await
              let n = length ys
              let xs = map (fromIntegral . (*10)) [0..n]
              let ps = zip xs $ map fromIntegral ys
              let lines = zipWithM_ line ps (drop 1 ps)
              setTimeout 3000 $ render c $ stroke  lines
