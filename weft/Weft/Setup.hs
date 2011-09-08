{-#LANGUAGE OverloadedStrings, ExtendedDefaultRules  #-}
module Weft.Setup (
  PrinterConfig (..),
  defaultConfig,
  printModel,
  raft  
  ) where
import Weft.Monad
import Control.Monad
import Weft.Commands
import Weft.Points

default(Double, Int)

printModel::PrinterConfig->Print ()->Print ()
printModel c model = initialize c >> home >> wait >> model >> end c

data PrinterConfig = Config { extSpeed::Double,
                              extTemp::Int,
                              platTemp::Int,
                              layer::Double
                            } deriving(Show,Eq)                                  
                                      
defaultConfig = Config {extSpeed = 1.98, extTemp = 220, platTemp = 125, layer = 0.35}

initialize conf = do
  comment "initialization - set modes, temperature etc"
  setMilimeters
  absolutePosition
  extruderOff
  toolSpeed 0 $ extSpeed conf
  extruderTemp 0 $ extTemp conf
  platformTemp 0 $ platTemp conf
  setFeedrate 3300
  
home = do
  comment "homing"
  homeMax "Z" 500
  emit "G92 Z10"
  emit "G1 Z0.0"
  homeMax "Z" 100
  homeMin "Y" 2500
  homeMin "X" 2500
  emit "M132 X Y Z A B"
wait = do
  comment "waiting for things to heat up"
  withRate 2400 $ move ((52,-57,10)::Point3) 
  waitForTemp 0
  extruderForward
  pause 5000
  extruderOff
             
end::PrinterConfig->Print ()
end c = do
  comment "shutting down"
  platformTemp 0 95
  withRate 2400 $ mapM_ move ([(6.18,4.11,5.1),(5.25,4.0,5.1),(4.91,4.33,5.1)]::[Point3])
  withRate 3300 $ move ((0,55)::Point2)
  toolSpeed 0 $ extSpeed c
  extruderReverse
  pause 2000
  extruderOff
  steppersOff
  waitForTemp 0
  conveyorOn
  pause 14000
  conveyorOff
  platformTemp 0 0
  extruderTemp 0 0

-- Takes two corners (smaller values first) and prints a raft.
raft::PrinterConfig->Point2->Point2->Print ()
raft conf s@(x,y) e = do
  comment "raft"
  withRate 350 $ do -- Go back and forth laying down thick lines with 1.5mm between their centers.
    move ((0,0,0.1+layer conf)::Point3) 
    extruderForward
    mapM_ move $ map (s<+>) $ zig 2 e
  withRate 1750 $ do -- The time for the thinner lines
    moveRel ((0,0,layer conf)::Point3) -- Move up before thin lines
    (x',_,_) <- getLocation
    mapM_ move  $ map (\(y',x)-> (x+x',y+y')) $  zig  1.5 ((0,1)<.>e,x-x')
  extruderOff
  
zig::Double->Point2->[Point2]
zig dx (x',dy) = zipWith (<+>) [(x,y)| x <- [0,dx..x'],y<-[0,0]] $ cycle  [(0,0),(0,dy),(0,dy),(0,0)] 
