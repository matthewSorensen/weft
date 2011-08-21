{-# LANGUAGE OverloadedStrings #-}
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
home = do
  comment "homing"
  homeMax "Z" 500
  emit "G92 Z10"
  emit "G1 Z0.0"
  homeMax "Z" 100
  homeMin "X" 2500
  homeMin "Y" 2500
  emit "M132 X Y Z A B"
wait = do
  comment "waiting for things to heat up"
  move ((52,-57,10)::Point3) (Just 3300)
  waitForTemp 0
  extruderForward
  pause 5000
  extruderOff
             
end::PrinterConfig->Print ()
end c = do
  comment "shutting down"
  platformTemp 0 95
  mapM_ (flip move $ Just 2400) ([(6.18,4.11,5.1),(5.25,4.0,5.1),(4.91,4.33,5.1)]::[Point3])
  move ((0,55)::Point2) $ Just 3300
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
raft conf (x,y) (x',y') = do
  comment "raft"
  setFeedrate 270
  move ((0,0,layer conf)::Point3) Nothing
  extruderForward
  -- Go back and forth laying down thick lines with 1.5mm between their centers.
  mapM_ (flip move Nothing) ([(i+j,k) | i<-[x,3+x..x'],(j,k)<-[(0,y),(0,y'),(1.5,y'),(1.5,y)], (i+1.5) <= x']::[Point2])
  -- The extruder is now at (<some x>,y), and it's time for the thinner lines
  moveRel ((0,0,layer conf)::Point3) Nothing
  setFeedrate 1750
  (x',_,_) <- getLocation
  mapM_ (flip move Nothing) ([(k,i+j) | i<-[y,2+y..y'],(j,k)<-[(0,x'),(0,x),(1,x),(1,x')], (i+1) <= y']::[Point2])
