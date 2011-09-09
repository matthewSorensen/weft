module Robotics.Thingomatic where
  
import qualified Data.ByteString as B
import Robotics.Thingomatic.Commands
import Robotics.Thingomatic.Setup
import Robotics.Thingomatic.Monad



demo = runWithIOAction B.putStrLn $ (printModel defaultConfig $ raft defaultConfig (10,10) (30,30))