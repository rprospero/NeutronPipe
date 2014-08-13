module Detector (detector) where

import Pipes
import Neutron

import Control.Monad(forever)
import Data.Default.Class (def)

import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Gtk
import Data.Colour
import Data.Colour.Names
import Control.Lens

import Data.Vector.Unboxed ((!),(//),replicate)

detector :: (Num a, Show a) => Consumer (Neutron a) IO ()
detector = forever $ do
  temp <- await
  lift $ print temp

norm :: Double -> Double
norm = exp . (* (-1) ) . (\x -> x*x)

chart :: Renderable ()
chart = toRenderable layout
    where
      layout = layout_title .~ "Amplitude Modulation"
           $ layout_plots .~ [toPlot sinusoid1]
           $ def
      sinusoid1 = plot_lines_values .~ [[ (x,(norm x)) | x <- [-4,(-3.995)..4]]]
              $ plot_lines_style  . line_color .~ opaque blue
              $ plot_lines_title .~ "am"
              $ def
incrIndex index value = value // [(index,1+(value!index))]

binner
  :: (RealFrac s) => Int -> [s] -> s -> Int
binner size xs value = floor $ (fromIntegral size) * (value - small) / ((large-small) * 101/100)
    where
      small = Prelude.minimum xs
      large = Prelude.maximum xs

histlist size xs = Prelude.foldr (incrIndex . f) (Data.Vector.Unboxed.replicate size 0) $ xs
                where f = binner size xs

test :: IO ()
test = renderableToWindow chart 100 100
