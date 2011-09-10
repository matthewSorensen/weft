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
  pause  125

layers = withRate 1500 $ mapM_ (const layer) [1..100]
  where layer = comment "Layer" >> moveRel ((0,0,0.35)::Point3) >>  mapM_ move points 

main = driver $ printModel $ raft (20,20) (20,20) >> start >> layers