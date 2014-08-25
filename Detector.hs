module Detector (detector,dumpToFile) where

import Pipes
import Pipes.Lift
import Neutron
import Vec

import Control.Monad(forever)
import Data.Default.Class (def)
import Control.Monad.Trans.State.Strict

import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Gtk
import Data.Colour
import Data.Colour.Names
import Control.Lens

import Data.Vector.Unboxed ((!),(//),replicate,toList)

import System.IO

detector :: (Num a, Show a) => Consumer (Neutron a) IO ()
detector = forever $ do
  temp <- await
  lift $ print temp

dumpToFile :: (Show a) => FilePath -> Consumer a IO ()
dumpToFile file = forever $ do
                    temp <- await
                    h <- lift $ openFile file AppendMode
                    lift $ hPutStr h . (++ "\n") . show $ temp
                    lift $ hClose h

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

binner' :: Monad m => Int -> Pipe a [a] (StateT [a] m) b
binner' bins = forever $
         do
           next <- await
           current <- lift $ get
           if length current >= bins
           then do
             lift $ put $ []
             yield . reverse $ [next] ++ current
           else do
             lift $ put $ [next] ++ current

binner :: Monad m => Int -> Pipe t [t] m r
binner bins = evalStateP [] $ binner' bins

pipeOver' :: Monad m => (a -> b -> b) -> Pipe a b (StateT b m )r
pipeOver' f =  forever $
         do
           next <- await
           current <- lift $ get
           let result = f next current
           lift $ put $ result
           yield result


pipeOver :: Monad m => b -> (a -> b -> b) -> 
            Pipe a b m r
pipeOver initial = evalStateP initial . pipeOver'


test chart = renderableToWindow chart 100 100


-- histogram size xs = toRenderable layout
--     where
--       layout = layout_title .~ "Amplitude Modulation"
--            $ layout_plots .~ [plotBars sinusoid1]
--            $ def
--       sinusoid1 = plot_bars_values .~ (zip [0..size] $  map (\x -> [x]) histed)
--               $ def
--       histed = (toList . histlist size $ xs) :: [Int]



xcoord :: Pipe (Neutron a) a IO ()
xcoord = forever $ do
           temp <- await
           yield $ x . position $ temp

