module Detector (detector,dumpToFile,dumpToConsole,pipeOver,xcoord,binner) where

import Pipes
import Pipes.Lift
import Neutron
import Vec

import Control.Monad(forever)
import Control.Monad.Trans.State.Strict

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

dumpToConsole :: (Show a) => Consumer a IO ()
dumpToConsole = forever $ do
                  temp <- await
                  lift $ print . (++ "\n") . show $ temp

binner' :: Monad m => Int -> Pipe a [a] (StateT [a] m) b
binner' bins = forever $
         do
           next <- await
           current <- lift get
           if length current >= bins
           then do
             lift $ put []
             yield . reverse $ next : current
           else
               lift $ put $ next : current

binner :: Monad m => Int -> Pipe t [t] m r
binner bins = evalStateP [] $ binner' bins

pipeOver' :: Monad m => (a -> b -> b) -> Pipe a b (StateT b m )r
pipeOver' f =  forever $
         do
           next <- await
           current <- lift get
           let result = f next current
           lift $ put result
           yield result


pipeOver :: Monad m => b -> (a -> b -> b) -> 
            Pipe a b m r
pipeOver initial = evalStateP initial . pipeOver'

xcoord :: Pipe (Neutron a) a IO ()
xcoord = forever $ do
           temp <- await
           yield $ x . position $ temp

