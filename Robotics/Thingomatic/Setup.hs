{-#LANGUAGE OverloadedStrings #-}
module Robotics.Thingomatic.Setup (
  printModel,
  raft  
  ) where
import Robotics.Thingomatic.Monad
import Control.Monad
import Robotics.Thingomatic.Commands
import Robotics.Thingomatic.Config
import Robotics.Thingomatic.Points


printModel::Print ()->Print ()
printModel m = withConfig (\c-> initialize c >> home >> wait >> m >> end c)

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
  extruderOff
  platformTemp 0 95
  (x,y,_) <- getLocation
  withRate 3500 $ move (x,y,100::Double)
  toolSpeed 0 $ extSpeed c
  extruderReverse
  pause 2000
  extruderOff
  extruderTemp 0 0
  move ((0,50)::Point2)
  conveyorOn
  pause 14000
  conveyorOff 
  platformTemp 0 0
  waitForTemp 0
  
-- Takes two corners (smaller values first) and prints a raft.
raft::Point2->Point2->Print ()
raft s@(x,y) e = do
  comment "raft"
  thick <- fmap layer getConfig
  withRate 350 $ do -- Go back and forth laying down thick lines with 2mm between their centers.
    move ((0,0,0.1+thick)::Point3) 
    extruderForward
    mapM_ move $ map (s<+>) $ zig 2 e
  withRate 1750 $ do -- The time for the thinner lines
    moveRel ((0,0,thick)::Point3) -- Move up before thin lines
    (x',_,_) <- getLocation
    mapM_ move  $ map (\(y',x)-> (x+x',y+y')) $  zig  1.5 ((0,1)<.>e,x-x')
  extruderOff
  
zig::Double->Point2->[Point2]
zig dx (x',dy) = zipWith (<+>) [(x,y)| x <- [0,dx..x'],y<-[0,0]] $ cycle  [(0,0),(0,dy),(0,dy),(0,0)] 
