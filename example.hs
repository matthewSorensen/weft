import Robotics.Thingomatic
import Robotics.Thingomatic.Commands
import Robotics.Thingomatic.Monad
import Robotics.Thingomatic.Points
import Robotics.Thingomatic.Setup
import Control.Monad


points::[Point2]
points = [(25,25),(25,35),(35,35),(35,25)]

start = do
  move $ head points
  extruderForward
  pause  250

layers = withRate 1500 $ mapM_ (const layer) [1..100]
  where layer = comment "Layer" >> moveRel ((0,0,0.35)::Point3) >>  mapM_ move points 

finish = do
  comment "Not crushing the model"
  extruderReverse
  (x,y,_) <- getLocation
  move ((x,y,10)::Point3)
  pause 1000
  extruderOff
  pause (10*1000)

main = driver $ printModel $ raft (20,20) (20,20) >> start >> layers >> finish